#[All_2024]
#Injury TTT 
#Accuracy — TTT: 65.47% | Book: 69.11% (same 2609 matches)
#Brier — TTT: 0.2257 | Book: 0.1990
#LogLoss — TTT: 0.6603 | Book: 0.5810
#CalRatio — TTT: 0.5822 | Book: 0.5957

#[ATP250]
#Injury TTT 
#Accuracy — TTT: 61.96% | Book: 66.12% (same 1033 matches)
#Brier — TTT: 0.2466 | Book: 0.2141
#LogLoss — TTT: 0.7092 | Book: 0.6166
#CalRatio — TTT: 0.5552 | Book: 0.5662

#[ATP500]
#Injury TTT 
#Accuracy — TTT: 65.21% | Book: 67.53% (same 388 matches)
#Brier — TTT: 0.2203 | Book: 0.2018
#LogLoss — TTT: 0.6440 | Book: 0.5875
#CalRatio — TTT: 0.5802 | Book: 0.5902

#[Masters1000]
#Injury TTT 
#Accuracy — TTT: 65.55% | Book: 67.73% (same 688 matches)
#Brier — TTT: 0.2241 | Book: 0.2077
#LogLoss — TTT: 0.6558 | Book: 0.6023
#CalRatio — TTT: 0.5835 | Book: 0.5878

#[Grandslams]
#Injury TTT 
#Accuracy — TTT: 72.80% | Book: 78.40% (same 500 matches)
#Brier — TTT: 0.1891 | Book: 0.1539
#LogLoss — TTT: 0.5781 | Book: 0.4731
#CalRatio — TTT: 0.6379 | Book: 0.6719

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

# ----------------------------
# FIXED PARAMETERS (LOCKED)
# ----------------------------
GAP_DAYS <- 14
ALPHA    <- 0.25
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


cal_ratio <- function(p_winner) {
  mean(p_winner, na.rm = TRUE)
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
    filter(year(Date) == 2024, ATP == k)
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
tournament_sets <- list(
  All_2024   = function(d) d,
  ATP250     = function(d) d %>% filter(str_detect(toupper(Series), "ATP 250|ATP250")),
  ATP500     = function(d) d %>% filter(str_detect(toupper(Series), "ATP 500|ATP500")),
  Masters1000 = function(d) d %>% filter(str_detect(toupper(Series), "MASTERS")),
  Grandslams  = function(d) d %>% filter(str_detect(toupper(Series), "GRAND SLAM"))
)

print_set_metrics <- function(d, set_name) {
  n  <- nrow(d)
  if (n == 0) { cat(sprintf("[%s]\nNo matches found.\n\n", set_name)); return(invisible(NULL)) }
  
  acc_ttt  <- 100 * mean(d$ttt_correct,   na.rm = TRUE)
  acc_book <- 100 * mean(d$bookie_correct, na.rm = TRUE)
  
  brier_ttt  <- brier_score(d$p_ttt,  d$y)
  brier_book <- brier_score(d$p_book, d$y)
  
  ll_ttt  <- log_loss(d$p_ttt,  d$y)
  ll_book <- log_loss(d$p_book, d$y)
  
  cal_ttt  <- cal_ratio(d$p_ttt)
  cal_book <- cal_ratio(d$p_book)
  
  cat(sprintf(
    "[%s]\nInjury TTT \nAccuracy — TTT: %.2f%% | Book: %.2f%% (same %d matches)\nBrier — TTT: %.4f | Book: %.4f\nLogLoss — TTT: %.4f | Book: %.4f\nCalRatio — TTT: %.4f | Book: %.4f\n\n",
    set_name,
    acc_ttt, acc_book, n,
    brier_ttt, brier_book,
    ll_ttt, ll_book,
    cal_ttt, cal_book
  ))
}

for (nm in names(tournament_sets)) {
  dsub <- tournament_sets[[nm]](comp_df)
  print_set_metrics(dsub, nm)
}