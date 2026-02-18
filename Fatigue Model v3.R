# A tibble: 5 × 10
#tournament_set acc_ttt_fat acc_book brier_ttt_fat brier_book logloss_ttt_fat logloss_book calratio_ttt
#<fct>                <dbl>    <dbl>         <dbl>      <dbl>           <dbl>        <dbl>        <dbl>
#1 Grandslams            72.6     78.4         0.224      0.154           0.942        0.473         1.23
#2 All_2024_R1           64.3     69.1         0.276      0.199           1.11         0.581         1.33
#3 ATP500                64.2     67.5         0.274      0.202           1.14         0.588         1.34
#4 ATP250                60.1     66.1         0.296      0.214           1.04         0.617         1.35
#5 Masters1000           64.7     67.7         0.284      0.208           1.32         0.602         1.36


library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

MAX_SETS    <- 5
DELTA       <- 0.75
WINDOW_DAYS <- 3
EPS         <- 1e-6

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

games_from_setcols <- function(df, w_prefix = "W", l_prefix = "L", max_sets = 5) {
  w_cols <- paste0(w_prefix, 1:max_sets)
  l_cols <- paste0(l_prefix, 1:max_sets)
  
  w_cols <- w_cols[w_cols %in% names(df)]
  l_cols <- l_cols[l_cols %in% names(df)]
  if (length(w_cols) == 0 || length(l_cols) == 0) {
    stop("No set columns found. Expected columns like W1/L1, W2/L2, ...")
  }
  
  w_mat <- as.matrix(df[, w_cols, drop = FALSE])
  l_mat <- as.matrix(df[, l_cols, drop = FALSE])
  
  suppressWarnings({
    w_mat <- apply(w_mat, 2, as.numeric)
    l_mat <- apply(l_mat, 2, as.numeric)
  })
  
  w_mat[is.na(w_mat)] <- 0
  l_mat[is.na(l_mat)] <- 0
  
  rowSums(w_mat + l_mat)
}

build_fatigue_tbl <- function(df_matches, delta = 0.75, window_days = 3) {
  player_day_games <- bind_rows(
    df_matches %>% transmute(Date, Player = Winner, GamesDay = GamesMatch),
    df_matches %>% transmute(Date, Player = Loser,  GamesDay = GamesMatch)
  ) %>%
    group_by(Player, Date) %>%
    summarise(GamesDay = sum(GamesDay, na.rm = TRUE), .groups = "drop")
  
  player_dates <- bind_rows(
    df_matches %>% transmute(Player = Winner, Date),
    df_matches %>% transmute(Player = Loser,  Date)
  ) %>% distinct()
  
  fatigue_tbl <- player_dates %>%
    tidyr::crossing(DayLag = 1:window_days) %>%
    mutate(DateLag = Date - lubridate::days(DayLag),
           w = delta^DayLag) %>%
    left_join(player_day_games, by = c("Player" = "Player", "DateLag" = "Date")) %>%
    mutate(GamesDay = coalesce(GamesDay, 0)) %>%
    group_by(Player, Date) %>%
    summarise(Fatigue = sum(w * GamesDay), .groups = "drop")
  
  fatigue_tbl
}

p_ttt_from_params <- function(mu_w, mu_l, s_w, s_l) {
  z <- (mu_w - mu_l) / sqrt(pmax(s_w^2 + s_l^2, 1e-9))
  pnorm(z)
}

fit_theta_offset <- function(df_train_with_probs_and_fat) {
  df_fit <- df_train_with_probs_and_fat %>%
    mutate(p_ttt_win = pmin(pmax(p_ttt_win, EPS), 1 - EPS)) %>%
    select(p_ttt_win, Fatigue_W, Fatigue_L) %>%
    bind_rows(
      transmute(., y = 1,
                p_ttt = p_ttt_win,
                fat_diff = Fatigue_W - Fatigue_L),
      transmute(., y = 0,
                p_ttt = 1 - p_ttt_win,
                fat_diff = Fatigue_L - Fatigue_W)
    ) %>%
    mutate(p_ttt = pmin(pmax(p_ttt, EPS), 1 - EPS))
  
  fit <- glm(y ~ fat_diff + offset(qlogis(p_ttt)),
             data = df_fit,
             family = binomial())
  
  -coef(fit)[["fat_diff"]]
}

# ----------------------------
# Load + prep
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    Court   = trimws(Court),
    Series  = trimws(as.character(Series)),
    Round   = trimws(as.character(Round)),
    WRank   = suppressWarnings(as.numeric(WRank)),
    LRank   = suppressWarnings(as.numeric(LRank)),
    Wsets   = as.integer(Wsets),
    Lsets   = as.integer(Lsets)
  ) %>%
  mutate(GamesMatch = games_from_setcols(cur_data(), "W", "L", MAX_SETS))

fatigue_tbl <- build_fatigue_tbl(df0, delta = DELTA, window_days = WINDOW_DAYS)

skills <- read_csv("skills_snapshot_by_ATPv2.csv", show_col_types = FALSE) %>%
  mutate(
    player       = trimws(player),
    ATP          = as.numeric(ATP),
    mu_generic   = as.numeric(mu_generic),
    sigma_generic = as.numeric(sigma_generic)
  )

ks <- df0 %>%
  filter(year(Date) == 2024) %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

train_pool <- df0 %>% filter(year(Date) == 2023)
all_preds  <- list()

for (k in ks) {
  
  test_df <- df0 %>%
    filter(year(Date) == 2024, ATP == k,!is.na(WRank), !is.na(LRank))
  if (nrow(test_df) == 0) next
  
  # Snapshot skills for tournament k
  stats_wide <- skills %>%
    filter(ATP == k) %>%
    select(player, mu_generic, sigma_generic)
  
  # Training set = expanding train_pool
  train_df <- train_pool
  
  train_with_params <- train_df %>%
    left_join(stats_wide, by = c("Winner" = "player")) %>%
    left_join(stats_wide, by = c("Loser"  = "player"), suffix = c("_w","_l")) %>%
    mutate(
      mu_winner    = coalesce(mu_generic_w, 0),
      mu_loser     = coalesce(mu_generic_l, 0),
      sigma_winner = coalesce(sigma_generic_w, 4),
      sigma_loser  = coalesce(sigma_generic_l, 4),
      p_ttt_win    = p_ttt_from_params(mu_winner, mu_loser, sigma_winner, sigma_loser)
    ) %>%
    left_join(fatigue_tbl %>% rename(Fatigue_W = Fatigue),
              by = c("Winner" = "Player", "Date" = "Date")) %>%
    left_join(fatigue_tbl %>% rename(Fatigue_L = Fatigue),
              by = c("Loser"  = "Player", "Date" = "Date")) %>%
    mutate(
      Fatigue_W = coalesce(Fatigue_W, 0),
      Fatigue_L = coalesce(Fatigue_L, 0)
    )
  
  theta_hat <- fit_theta_offset(train_with_params)
  
  preds <- test_df %>%
    left_join(stats_wide, by = c("Winner" = "player")) %>%
    left_join(stats_wide, by = c("Loser"  = "player"), suffix = c("_w","_l")) %>%
    left_join(fatigue_tbl %>% rename(Fatigue_W = Fatigue),
              by = c("Winner" = "Player", "Date" = "Date")) %>%
    left_join(fatigue_tbl %>% rename(Fatigue_L = Fatigue),
              by = c("Loser"  = "Player", "Date" = "Date")) %>%
    mutate(
      Fatigue_W = coalesce(Fatigue_W, 0),
      Fatigue_L = coalesce(Fatigue_L, 0),
      
      mu_winner    = coalesce(mu_generic_w, 0),
      mu_loser     = coalesce(mu_generic_l, 0),
      sigma_winner = coalesce(sigma_generic_w, 4),
      sigma_loser  = coalesce(sigma_generic_l, 4),
      
      p_ttt_win = p_ttt_from_params(mu_winner, mu_loser, sigma_winner, sigma_loser),
      p_ttt_win = pmin(pmax(p_ttt_win, EPS), 1 - EPS),
      
      p_adj_win = plogis(qlogis(p_ttt_win) - theta_hat * (Fatigue_W - Fatigue_L)),
      
      # labels + correctness (Winner-vs-Loser framing)
      y = 1L,
      
      pred_base      = if_else(p_ttt_win >= 0.5, Winner, Loser),
      ttt_correct    = pred_base == Winner,
      
      pred_fat       = if_else(p_adj_win >= 0.5, Winner, Loser),
      ttt_fat_correct = pred_fat == Winner,
      
      # bookmaker implied prob Winner wins (normalized)
      p_book = case_when(
        !is.na(B365W) & !is.na(B365L) & B365W > 0 & B365L > 0 ~
          (1 / B365W) / ((1 / B365W) + (1 / B365L)),
        TRUE ~ NA_real_
      ),
      pred_book       = case_when(
        !is.na(p_book) & p_book >= 0.5 ~ Winner,
        !is.na(p_book) & p_book <  0.5 ~ Loser,
        TRUE ~ NA_character_
      ),
      bookie_correct  = !is.na(pred_book) & pred_book == Winner,
      
      # favorite framing for calibration ratio
      p_base_fav = pmax(p_ttt_win, 1 - p_ttt_win),
      y_base_fav = as.integer(p_ttt_win >= 0.5),
      
      p_fat_fav  = pmax(p_adj_win, 1 - p_adj_win),
      y_fat_fav  = as.integer(p_adj_win >= 0.5),
      
      p_book_fav = if_else(!is.na(p_book), pmax(p_book, 1 - p_book), NA_real_),
      y_book_fav = if_else(!is.na(p_book), as.integer(p_book >= 0.5), NA_integer_),
      
      theta_used = theta_hat,
      ATP_k      = k
    )
  
  all_preds[[length(all_preds) + 1]] <- preds
  train_pool <- bind_rows(train_pool, test_df)
}

all_df <- bind_rows(all_preds)

# ----------------------------
# Comparable matches filter
# ----------------------------
comp_df <- all_df %>%
  filter(
    !is.na(B365W), !is.na(B365L),
    B365W > 0, B365L > 0,
    B365W != B365L,
    !is.na(p_ttt_win),
    !is.na(p_adj_win),
    !is.na(p_book),
    !is.na(ttt_correct),
    !is.na(ttt_fat_correct),
    !is.na(bookie_correct)
  )

# ----------------------------
# Tournament-set summaries
# ----------------------------
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
   
    
    acc_ttt_fat   = round(100 * mean(d$ttt_fat_correct, na.rm = TRUE), 2),
    acc_book      = round(100 * mean(d$bookie_correct, na.rm = TRUE), 2),
    
    brier_ttt_fat = round(brier_score(d$p_adj_win,  d$y), 4),
    brier_book    = round(brier_score(d$p_book,     d$y), 4),
    
    logloss_ttt_fat = round(log_loss(d$p_adj_win, d$y), 4),
    logloss_book    = round(log_loss(d$p_book,    d$y), 4),
    
    calratio_ttt     = round(cal_ratio(d$p_base_fav, d$y_base_fav), 4),
    calratio_ttt_fat = round(cal_ratio(d$p_fat_fav,  d$y_fat_fav), 4),
    calratio_book    = round(cal_ratio(d$p_book_fav, d$y_book_fav), 4)
  )
}

final_results <- bind_rows(lapply(names(tournament_sets), function(nm) {
  dsub <- tournament_sets[[nm]](comp_df)
  summarise_set(dsub, nm)
})) %>%
  mutate(tournament_set = factor(
    tournament_set,
    levels = c("Grandslams","All_2024_R1","ATP500","ATP250","Masters1000")
  )) %>%
  arrange(tournament_set)

print(final_results)

# ----------------------------
# (Optional) keep your disagreement exports
# ----------------------------
disagree_baseline <- comp_df %>% filter(bookie_correct & !ttt_correct)
disagree_fatigue  <- comp_df %>% filter(bookie_correct & !ttt_fat_correct)

write_csv(disagree_baseline, "bet365_right_ttt_wrong_BASELINE_v2.csv")
write_csv(disagree_fatigue,  "bet365_right_ttt_wrong_FATIGUE_optionA_v2.csv")

cat("Saved:\n")
cat(" - bet365_right_ttt_wrong_BASELINE_v2.csv\n")
cat(" - bet365_right_ttt_wrong_FATIGUE_optionA_v2.csv\n")
