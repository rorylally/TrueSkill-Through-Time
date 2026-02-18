#Accuracy — TTT: 62.46% | TTT+Fatigue: 62.72% | Bookmakers: 68.37% (same 1132 matches)

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)

MAX_SETS    <- 5
DELTA       <- 0.75
WINDOW_DAYS <- 3
EPS         <- 1e-6



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


df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    Court   = trimws(Court)
  ) %>%
  mutate(GamesMatch = games_from_setcols(cur_data(), "W", "L", MAX_SETS))

fatigue_tbl <- build_fatigue_tbl(df0, delta = DELTA, window_days = WINDOW_DAYS)


skills <- read_csv("skills_snapshot_by_ATP.csv", show_col_types = FALSE) %>%
  mutate(player = trimws(player))


ks <- df0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

train_pool <- df0 %>% filter(year(Date) == 2023)  # for defining "what is training" per k
all_preds <- list()

for (k in ks) {
  
  test_df <- df0 %>%
    filter(year(Date) == 2024, ATP == k, Round == "1st Round")
  if (nrow(test_df) == 0) next
  
  # Snapshot skills JUST BEFORE tournament k
  stats_wide <- skills %>%
    filter(ATP == k) %>%
    select(player, mu_generic, sigma_generic)
  
  # --- Build training set = whatever was in train_pool so far (same as your expanding design) ---
  train_df <- train_pool
  
  # Compute p_ttt for training matches using the snapshot skills for k
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
  
  # Predict test matches
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
      
      pred        = if_else(p_ttt_win >= 0.5, Winner, Loser),
      ttt_correct = pred == Winner,
      
      pred_fat        = if_else(p_adj_win >= 0.5, Winner, Loser),
      ttt_fat_correct = pred_fat == Winner,
      
      bookie_pred = case_when(
        !is.na(B365W) & !is.na(B365L) & B365W < B365L ~ Winner,
        !is.na(B365W) & !is.na(B365L) & B365W > B365L ~ Loser,
        TRUE ~ NA_character_
      ),
      bookie_correct = !is.na(bookie_pred) & bookie_pred == Winner,
      
      theta_used = theta_hat,
      ATP_k = k
    )
  
  all_preds[[length(all_preds) + 1]] <- preds
  
  # Expand training pool (so next k has more "past data")
  train_pool <- bind_rows(train_pool, test_df)
}

all_df <- bind_rows(all_preds)

# ============================================================
# 5) EVALUATION ON SAME COMPARABLE MATCHES
# ============================================================
comp_df <- all_df %>%
  filter(
    !is.na(B365W), !is.na(B365L),
    B365W > 0, B365L > 0,
    B365W != B365L,
    !is.na(ttt_correct),
    !is.na(ttt_fat_correct),
    !is.na(bookie_correct)
  )

ttt_acc     <- mean(comp_df$ttt_correct) * 100
ttt_fat_acc <- mean(comp_df$ttt_fat_correct) * 100
book_acc    <- mean(comp_df$bookie_correct) * 100
n_comp      <- nrow(comp_df)

cat(sprintf(
  "Accuracy — TTT: %.2f%% | TTT+Fatigue: %.2f%% | Bookmakers: %.2f%% (same %d matches)\n",
  ttt_acc, ttt_fat_acc, book_acc, n_comp
))

disagree_baseline <- comp_df %>% filter(bookie_correct & !ttt_correct)
disagree_fatigue  <- comp_df %>% filter(bookie_correct & !ttt_fat_correct)

write_csv(disagree_baseline, "bet365_right_ttt_wrong_BASELINE_v2.csv")
write_csv(disagree_fatigue,  "bet365_right_ttt_wrong_FATIGUE_optionA_v2.csv")

accuracy_summary <- tibble(
  ttt_accuracy          = round(ttt_acc, 2),
  ttt_fatigue_accuracy  = round(ttt_fat_acc, 2),
  bookmaker_accuracy    = round(book_acc, 2),
  n_matches             = n_comp,
  delta                 = DELTA,
  window_days           = WINDOW_DAYS
)
write_csv(accuracy_summary, "accuracy_summary_FATIGUE_optionA_v2.csv")

cat("Saved:\n")
cat(" - bet365_right_ttt_wrong_BASELINE_v2.csv\n")
cat(" - bet365_right_ttt_wrong_FATIGUE_optionA_v2.csv\n")
cat(" - accuracy_summary_FATIGUE_optionA_v2.csv\n")
