#Top 100
# A tibble: 5 × 11
#tournament_set n_tournaments_used n_matches acc_ttt acc_book brier_ttt brier_book logloss_ttt logloss_book calratio_ttt
#<fct>                       <int>     <int>   <dbl>    <dbl>     <dbl>      <dbl>       <dbl>        <dbl>        <dbl>
#1 Grandslams                      4       327    74.6     79.2     0.174      0.152       0.525        0.471        0.961
#2 All_2024_R1                    65      1680    65.2     68.2     0.216      0.201       0.621        0.585        1.04 
#3 ATP500                         12       289    61.9     64.4     0.224      0.211       0.638        0.605        1.09 
#4 ATP250                         39       524    60.5     63.6     0.237      0.220       0.668        0.631        1.08 
#5 Masters1000                    10       540    65.9     68.0     0.218      0.207       0.623        0.599        1.05 

#tournament_set n_tournaments_used n_matches acc_ttt acc_book brier_ttt brier_book logloss_ttt logloss_book calratio_ttt
#<fct>                       <int>     <int>   <dbl>    <dbl>     <dbl>      <dbl>       <dbl>        <dbl>        <dbl>
#1 Grandslams                      4       499    73.0     78.4     0.189      0.154       0.587        0.473         1.02
#2 All_2024_R1                    65      2607    65.0     69.1     0.227      0.199       0.670        0.581         1.09
#3 ATP500                         12       388    63.9     67.5     0.221      0.202       0.649        0.588         1.08
#4 ATP250                         39      1032    61.1     66.1     0.249      0.214       0.724        0.617         1.14
#5 Masters1000                    10       688    65.7     67.7     0.224      0.208       0.660        0.602         1.07

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

# ----------------------------
# FIXED PARAMETERS (LOCKED)
# ----------------------------
GAP_DAYS <- 16
ALPHA    <- 0.5
BETA     <- 1.0

MATCH_FILE  <- "2023_2024_ATP_Results.xlsx"
SKILLS_FILE <- "skills_snapshot_by_ATPv2.csv"

to_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct","POSIXlt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  as.Date(as.character(x))
}

brier_score <- function(p, y) mean((p - y)^2, na.rm = TRUE)

log_loss <- function(p, y) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
}

cal_ratio <- function(p_fav, y_fav) {
  denom <- sum(y_fav, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  sum(p_fav, na.rm = TRUE) / denom
}

# ----------------------------
# 1) Load match data
# ----------------------------
df0 <- read_excel(MATCH_FILE) %>%
  mutate(
    Date    = to_date_safe(Date),
    Comment = if_else(is.na(Comment), "", as.character(Comment)),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    Court   = trimws(Court),
    Series  = trimws(as.character(Series)),
    Round   = trimws(as.character(Round)),
    WRank = suppressWarnings(as.numeric(WRank)),
    LRank = suppressWarnings(as.numeric(LRank)),
    B365W = suppressWarnings(as.numeric(B365W)),
    B365L = suppressWarnings(as.numeric(B365L))
  )%>%
  filter(
    Comment == "Completed"
  ) 

# tournaments to evaluate (same as before)
ks <- df0 %>%
  filter(year(Date) == 2024) %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ----------------------------
# 2) Load skills snapshot (NO Date column here)
# Expect columns: player, mu_generic, sigma_generic, ATP
# ----------------------------
skills0 <- read_csv(SKILLS_FILE, show_col_types = FALSE) %>%
  mutate(
    player      = trimws(player),
    ATP         = as.numeric(ATP),
    mu_generic  = as.numeric(mu_generic),
    sigma_generic = as.numeric(sigma_generic)
  )

# ----------------------------
# 3) Absence table (tournament-specific, based on expanding train_pool)
# ----------------------------
build_absence_tbl <- function(train_pool, players_needed, anchor_date, GAP_DAYS) {
  
  last_played <- train_pool %>%
    select(Date, Winner, Loser) %>%
    pivot_longer(cols = c(Winner, Loser), names_to = "role", values_to = "player") %>%
    mutate(player = trimws(player)) %>%
    group_by(player) %>%
    summarise(last_match_date = max(Date), .groups = "drop")
  
  tibble(player = players_needed) %>%
    left_join(last_played, by = "player") %>%
    mutate(
      days_since_play = as.numeric(anchor_date - last_match_date),
      inj_flag = case_when(
        is.na(days_since_play) ~ 0L,
        days_since_play < 0 ~ 0L,
        days_since_play >= GAP_DAYS ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    select(player, inj_flag, days_since_play)
}

# ----------------------------
# 4) Skills lookup by ATP (tournament code)
# ----------------------------
get_skills_for_atp <- function(atp_code, players_vec, skills_tbl) {
  s <- skills_tbl %>% filter(ATP == atp_code)
  
  tibble(player = players_vec) %>%
    left_join(s %>% select(player, mu_generic, sigma_generic), by = "player") %>%
    mutate(
      mu_generic    = coalesce(mu_generic, 0),
      sigma_generic = coalesce(sigma_generic, 4)
    )
}

# ----------------------------
# 5) Run final evaluation (no tuning)
# ----------------------------
train_pool <- df0 %>% filter(year(Date) == 2023)
all_preds  <- list()

for (k in ks) {
  
  test_df <- df0 %>%
    filter(year(Date) == 2024, ATP == k, !is.na(WRank), !is.na(LRank))
  if (nrow(test_df) == 0) next
  
  anchor_date <- min(test_df$Date, na.rm = TRUE)
  players_needed <- unique(c(test_df$Winner, test_df$Loser))
  
  abs_tbl <- build_absence_tbl(
    train_pool     = train_pool,
    players_needed = players_needed,
    anchor_date    = anchor_date,
    GAP_DAYS       = GAP_DAYS
  )
  
  stats_wide <- get_skills_for_atp(
    atp_code    = k,
    players_vec = players_needed,
    skills_tbl  = skills0
  )
  
  preds <- test_df %>%
    left_join(stats_wide, by = c("Winner" = "player")) %>%
    rename(mu_w = mu_generic, sigma_w = sigma_generic) %>%
    left_join(stats_wide, by = c("Loser" = "player")) %>%
    rename(mu_l = mu_generic, sigma_l = sigma_generic) %>%
    left_join(abs_tbl, by = c("Winner" = "player")) %>%
    rename(inj_w = inj_flag) %>%
    left_join(abs_tbl, by = c("Loser" = "player")) %>%
    rename(inj_l = inj_flag) %>%
    mutate(
      inj_w = as.integer(coalesce(inj_w, 0L)),
      inj_l = as.integer(coalesce(inj_l, 0L)),
      
      mu_w_adj = mu_w - ALPHA * sigma_w * inj_w,
      mu_l_adj = mu_l - ALPHA * sigma_l * inj_l,
      
      z     = (mu_w_adj - mu_l_adj) / sqrt(2 * BETA^2 + sigma_w^2 + sigma_l^2),
      p_ttt = pnorm(z),                       # P(Winner wins)
      y     = 1L,                             # Winner always won (for Winner-vs-Loser framing)
      
      pred_ttt     = if_else(p_ttt >= 0.5, Winner, Loser),
      ttt_correct  = pred_ttt == Winner
    ) %>%
    mutate(
      # bookmaker implied prob that the Winner wins
      p_book = case_when(
        !is.na(B365W) & !is.na(B365L) & B365W > 0 & B365L > 0 ~
          (1 / B365W) / ((1 / B365W) + (1 / B365L)),
        TRUE ~ NA_real_
      ),
      pred_book     = case_when(
        !is.na(p_book) & p_book >= 0.5 ~ Winner,
        !is.na(p_book) & p_book <  0.5 ~ Loser,
        TRUE ~ NA_character_
      ),
      bookie_correct = !is.na(pred_book) & pred_book == Winner,
      
      # "favorite" framing for calibration ratio
      p_ttt_fav  = pmax(p_ttt, 1 - p_ttt),
      y_ttt_fav  = as.integer(p_ttt >= 0.5),  # if model favored Winner, favorite won; else favorite lost
      
      p_book_fav = if_else(!is.na(p_book), pmax(p_book, 1 - p_book), NA_real_),
      y_book_fav = if_else(!is.na(p_book), as.integer(p_book >= 0.5), NA_integer_)
    )
  
  all_preds[[length(all_preds) + 1]] <- preds
  
  # expanding pool (same as tuning script)
  train_pool <- bind_rows(train_pool, test_df)
}

all_df <- bind_rows(all_preds)

# ----------------------------
# 6) Evaluation filter (match your previous comp_df rules)
# ----------------------------
comp_df <- all_df %>%
  filter(
    !is.na(B365W), !is.na(B365L),
    B365W > 0, B365L > 0,
    B365W != B365L,
    !is.na(p_ttt),
    !is.na(p_book),
    !is.na(ttt_correct),
    !is.na(bookie_correct)
  )

# ----------------------------
# 7) Tournament-set summaries
# ----------------------------
# NOTE: if your Series labels differ (e.g., "Masters 1000" vs "Masters1000"),
# tweak the grepl() patterns below.
tournament_sets <- list(
  Grandslams   = function(d) d %>% filter(str_detect(toupper(Series), "GRAND SLAM")),
  Masters1000  = function(d) d %>% filter(str_detect(toupper(Series), "MASTERS")),
  ATP500       = function(d) d %>% filter(str_detect(toupper(Series), "ATP 500|ATP500")),
  ATP250       = function(d) d %>% filter(str_detect(toupper(Series), "ATP 250|ATP250")),
  All_2024_R1  = function(d) d
)

summarise_set <- function(d, set_name) {
  tibble(
    tournament_set     = set_name,
    n_tournaments_used = n_distinct(d$ATP),
    n_matches          = nrow(d),
    
    acc_ttt  = round(100 * mean(d$ttt_correct, na.rm = TRUE), 2),
    acc_book = round(100 * mean(d$bookie_correct, na.rm = TRUE), 2),
    
    brier_ttt  = round(brier_score(d$p_ttt,  d$y), 4),
    brier_book = round(brier_score(d$p_book, d$y), 4),
    
    logloss_ttt  = round(log_loss(d$p_ttt,  d$y), 4),
    logloss_book = round(log_loss(d$p_book, d$y), 4),
    
    calratio_ttt  = round(cal_ratio(d$p_ttt_fav,  d$y_ttt_fav), 4),
    calratio_book = round(cal_ratio(d$p_book_fav, d$y_book_fav), 4)
  )
}

final_results <- bind_rows(lapply(names(tournament_sets), function(nm) {
  dsub <- tournament_sets[[nm]](comp_df)
  summarise_set(dsub, nm)
})) %>%
  # nice ordering like your example
  mutate(tournament_set = factor(tournament_set,
                                 levels = c("Grandslams","All_2024_R1","ATP500","ATP250","Masters1000"))) %>%
  arrange(tournament_set)

print(final_results)

# OPTIONAL: also print injury subset stats per set (if you still want them)
# inj_subset <- comp_df %>% filter(inj_w == 1L | inj_l == 1L)
