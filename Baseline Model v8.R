library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(tibble)


# ----------------------------
# 0) Tournament sets (your input)
# ----------------------------
TOURNAMENT_SETS <- list(
  All_2024_R1 = NULL,
  ATP250 = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27, 30, 31, 33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53, 57, 58, 59, 63, 64),
  ATP500 = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60, 61),
  Masters1000 = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams = c(5, 32, 39, 51)
)

# ----------------------------
# 1) Proper scoring rules
# ----------------------------
brier_score <- function(p, y) mean((p - y)^2, na.rm = TRUE)

log_loss <- function(p, y) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
}

# ----------------------------
# 2) Load data
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Year    = year(Date),
    Winner  = str_trim(Winner),
    Loser   = str_trim(Loser),
    Surface = str_trim(Surface),
    Court   = str_trim(Court),
    Round   = str_trim(Round),
    WRank = suppressWarnings(as.numeric(WRank)),
    LRank = suppressWarnings(as.numeric(LRank)),
    B365W = suppressWarnings(as.numeric(B365W)),
    B365L = suppressWarnings(as.numeric(B365L))
  )

skills <- read_csv("skills_snapshot_by_ATPv2.csv", show_col_types = FALSE) %>%
  mutate(player = str_trim(player))

# ----------------------------
# 3) Base tournament list available in 2024 R1
# ----------------------------
ks_all <- df0 %>%
  filter(Year == 2024) %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ----------------------------
# 4) Runner for one set of tournaments (TTT snapshots)
# ----------------------------
run_ttt_for_set <- function(set_name, atp_vec_or_null) {

  ks <- if (is.null(atp_vec_or_null)) {
    ks_all
  } else {
    sort(intersect(ks_all, atp_vec_or_null))
  }

  match_list <- list()
  ctr <- 1L

  for (k in ks) {

    test_df <- df0 %>%
      filter(Year == 2024, ATP == k)
    if (nrow(test_df) == 0) next

    stats_wide <- skills %>%
      filter(ATP == k) %>%
      select(player, mu_generic, sigma_generic)

    preds <- test_df %>%
      left_join(stats_wide, by = c("Winner" = "player")) %>%
      left_join(stats_wide, by = c("Loser"  = "player"), suffix = c("_w","_l")) %>%
      mutate(
        mu_winner    = coalesce(mu_generic_w, 0),
        mu_loser     = coalesce(mu_generic_l, 0),
        sigma_winner = coalesce(sigma_generic_w, 4),
        sigma_loser  = coalesce(sigma_generic_l, 4),

        # TTT probability from z
        z_val = (mu_winner - mu_loser) /
          sqrt(pmax(2 * 1.0^2 + sigma_winner^2 + sigma_loser^2, 1e-9)),
        p_win_ttt  = pnorm(z_val),
        p_lose_ttt = 1 - p_win_ttt,

        # Hard preds
        ttt_pred = if_else(p_win_ttt >= 0.5, Winner, Loser),
        ttt_correct = (ttt_pred == Winner),

        # Bookmaker implied prob (normalized overround)
        p_win_book = if_else(
          !is.na(B365W) & !is.na(B365L) & B365W > 0 & B365L > 0 & B365W != B365L,
          (1 / B365W) / ((1 / B365W) + (1 / B365L)),
          NA_real_
        ),
        p_lose_book = if_else(!is.na(p_win_book), 1 - p_win_book, NA_real_),

        book_pred = case_when(
          !is.na(B365W) & !is.na(B365L) & B365W < B365L ~ Winner,
          !is.na(B365W) & !is.na(B365L) & B365W > B365L ~ Loser,
          TRUE ~ NA_character_
        ),
        book_correct = !is.na(book_pred) & (book_pred == Winner),

        actual = 1,
        set_name = set_name,
        ATPk = k
      )

    # store
    for (j in seq_len(nrow(preds))) {
      match_list[[ctr]] <- preds[j, ] %>%
        transmute(
          Date,
          ATP,
          Tournament = if ("Tournament" %in% names(df0)) as.character(Tournament) else NA_character_,
          Round,
          Winner,
          Loser,
          mu_winner, mu_loser,
          sigma_winner, sigma_loser,
          z_val,
          p_win_ttt, p_lose_ttt,
          p_win_book, p_lose_book,
          ttt_correct, book_correct,
          actual,
          set_name,
          ATPk,
          B365W, B365L
        )
      ctr <- ctr + 1L
    }
  }

  all_preds <- bind_rows(match_list)

  # --- TTT metrics: all matches with a valid TTT prediction ---
  ttt_all <- all_preds %>%
    filter(!is.na(p_win_ttt), !is.na(ttt_correct))

  n_ttt <- nrow(ttt_all)
  acc_ttt   <- mean((ttt_all$p_win_ttt >= 0.5) == (ttt_all$actual == 1))
  brier_ttt <- brier_score(ttt_all$p_win_ttt, ttt_all$actual)
  ll_ttt    <- log_loss(ttt_all$p_win_ttt, ttt_all$actual)
  cal_ttt   <- sum(ttt_all$p_win_ttt) / sum(ttt_all$actual)

  # --- Bookmaker metrics: same-match comparison (requires valid odds) ---
  comp <- all_preds %>%
    filter(
      !is.na(B365W), !is.na(B365L),
      B365W > 0, B365L > 0,
      B365W != B365L,
      !is.na(p_win_book),
      !is.na(p_win_ttt),
      !is.na(ttt_correct),
      !is.na(book_correct)
    )

  n_comp <- nrow(comp)
  acc_ttt_comp   <- mean((comp$p_win_ttt >= 0.5) == (comp$actual == 1))
  brier_ttt_comp <- brier_score(comp$p_win_ttt, comp$actual)
  ll_ttt_comp    <- log_loss(comp$p_win_ttt, comp$actual)
  cal_ttt_comp   <- sum(comp$p_win_ttt) / sum(comp$actual)

  acc_book   <- mean((comp$p_win_book >= 0.5) == (comp$actual == 1))
  brier_book <- brier_score(comp$p_win_book, comp$actual)
  ll_book    <- log_loss(comp$p_win_book, comp$actual)
  cal_book   <- sum(comp$p_win_book) / sum(comp$actual)

  cat(sprintf(
    "[%s]\nTTT (all %d matches)\nAccuracy — TTT: %.2f%% | Brier: %.4f | LogLoss: %.4f | CalRatio: %.4f\n\nTTT vs Book (same %d matches with odds)\nAccuracy — TTT: %.2f%% | Book: %.2f%%\nBrier     — TTT: %.4f | Book: %.4f\nLogLoss   — TTT: %.4f | Book: %.4f\nCalRatio  — TTT: %.4f | Book: %.4f\n\n",
    set_name,
    n_ttt, 100*acc_ttt, brier_ttt, ll_ttt, cal_ttt,
    n_comp,
    100*acc_ttt_comp, 100*acc_book,
    brier_ttt_comp, brier_book,
    ll_ttt_comp, ll_book,
    cal_ttt_comp, cal_book
  ))

  write_csv(all_preds, sprintf("TTT_predictions_%s.csv", set_name))

  list(
    set_name        = set_name,
    ks              = ks,
    n_ttt           = n_ttt,
    acc_ttt         = 100 * acc_ttt,
    brier_ttt       = brier_ttt,
    logloss_ttt     = ll_ttt,
    calratio_ttt    = cal_ttt,
    n_comp          = n_comp,
    acc_ttt_comp    = 100 * acc_ttt_comp,
    acc_book        = 100 * acc_book,
    brier_ttt_comp  = brier_ttt_comp,
    brier_book      = brier_book,
    logloss_ttt_comp = ll_ttt_comp,
    logloss_book    = ll_book,
    calratio_ttt_comp = cal_ttt_comp,
    calratio_book   = cal_book
  )
}

# ----------------------------
# 5) Run all sets
# ----------------------------
results <- lapply(names(TOURNAMENT_SETS), function(nm) {
  run_ttt_for_set(nm, TOURNAMENT_SETS[[nm]])
})
