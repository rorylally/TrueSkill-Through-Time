library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)

beta <- 1
GAP_GRID   <- c(30)
ALPHA_GRID <- c(0.4)

TOURNAMENT_SETS <- list(
  All_2024_R1  = NULL,
  ATP250       = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27, 30, 31, 33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53, 57, 58, 59, 63, 64),
  ATP500       = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60, 61),
  Masters1000  = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams   = c(5, 32, 39, 51)
)

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

df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Comment = trimws(Comment),
    Series  = trimws(as.character(Series)),
    Round   = trimws(as.character(Round)),
    WRank   = suppressWarnings(as.numeric(WRank)),
    LRank   = suppressWarnings(as.numeric(LRank)),
    Wsets   = as.integer(Wsets),
    Lsets   = as.integer(Lsets)
  ) %>%
  filter(
    Comment == "Completed",
    !is.na(WRank), !is.na(LRank),
    WRank <= 100, LRank <= 100
  ) %>%
  mutate(
    margin = pmin(pmax(Wsets - Lsets, 1), 3)
  )

# ----------------------------
# Load ordinal skill snapshots
# Expected columns: ATP | player | mu_generic | sigma_generic
# ----------------------------
skills_ord <- read_csv("skills_snapshot_by_ATP_best_MOVv2.csv", show_col_types = FALSE) %>%
  mutate(
    player        = trimws(player),
    ATP           = as.numeric(ATP),
    mu_generic    = as.numeric(mu_generic),
    sigma_generic = as.numeric(sigma_generic)
  )

# ----------------------------
# All tournaments available in eval set
# ----------------------------
ks_all <- df0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ----------------------------
# Absence helper (tournament-specific, expanding train pool)
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
      abs_flag = case_when(
        is.na(days_since_play) ~ 0L,
        days_since_play < 0 ~ 0L,
        days_since_play >= GAP_DAYS ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    select(player, abs_flag, days_since_play)
}

# ----------------------------
# Run one combo (GAP_DAYS, ALPHA) on a given tournament set ks
# ALSO returns Brier + LogLoss + CalRatio for TTT and Book
# ----------------------------
run_one_combo <- function(GAP_DAYS, ALPHA, ks, set_name) {
  
  all_preds  <- list()
  
  # Seed expanding pool with 2023 matches
  train_pool <- df0 %>% filter(year(Date) == 2023)
  
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(year(Date) == 2024, ATP == k, Round == "1st Round")
    if (nrow(test_df) == 0) next
    
    anchor_date <- min(test_df$Date, na.rm = TRUE)
    players_needed <- unique(c(test_df$Winner, test_df$Loser))
    
    abs_tbl <- build_absence_tbl(
      train_pool     = train_pool,
      players_needed = players_needed,
      anchor_date    = anchor_date,
      GAP_DAYS       = GAP_DAYS
    )
    
    stats_wide <- skills_ord %>%
      filter(ATP == k) %>%
      select(player, mu_generic, sigma_generic)
    
    preds <- test_df %>%
      left_join(stats_wide, by = c("Winner" = "player")) %>%
      rename(mu_winner = mu_generic, sigma_winner = sigma_generic) %>%
      left_join(stats_wide, by = c("Loser"  = "player")) %>%
      rename(mu_loser  = mu_generic, sigma_loser  = sigma_generic) %>%
      left_join(abs_tbl, by = c("Winner" = "player")) %>%
      rename(abs_w = abs_flag) %>%
      left_join(abs_tbl, by = c("Loser"  = "player")) %>%
      rename(abs_l = abs_flag) %>%
      mutate(
        mu_winner    = coalesce(mu_winner, 0),
        mu_loser     = coalesce(mu_loser, 0),
        sigma_winner = coalesce(sigma_winner, 4),
        sigma_loser  = coalesce(sigma_loser, 4),
        
        abs_w = as.integer(coalesce(abs_w, 0L)),
        abs_l = as.integer(coalesce(abs_l, 0L)),
        
        # absence adjustment
        mu_w_adj = mu_winner - ALPHA * sigma_winner * abs_w,
        mu_l_adj = mu_loser  - ALPHA * sigma_loser  * abs_l,
        
        # ordinal-consistent probability
        denom = sqrt(pmax(2*beta^2 + sigma_winner^2 + sigma_loser^2, 1e-9)),
        z_val = (mu_w_adj - mu_l_adj) / denom,
        
        p_ttt = pmin(pmax(pnorm(z_val), 1e-15), 1 - 1e-15),  # P(Winner wins)
        y     = 1L,
        
        pred        = if_else(p_ttt >= 0.5, Winner, Loser),
        ttt_correct = pred == Winner,
        
        # bookmaker implied prob Winner wins (normalized)
        p_book = case_when(
          !is.na(B365W) & !is.na(B365L) & B365W > 0 & B365L > 0 ~
            (1 / B365W) / ((1 / B365W) + (1 / B365L)),
          TRUE ~ NA_real_
        ),
        pred_book = case_when(
          !is.na(p_book) & p_book >= 0.5 ~ Winner,
          !is.na(p_book) & p_book <  0.5 ~ Loser,
          TRUE ~ NA_character_
        ),
        bookie_correct = !is.na(pred_book) & pred_book == Winner,
        
        # favorite framing for calibration ratio
        p_ttt_fav  = pmax(p_ttt, 1 - p_ttt),
        y_ttt_fav  = as.integer(p_ttt >= 0.5),
        
        p_book_fav = if_else(!is.na(p_book), pmax(p_book, 1 - p_book), NA_real_),
        y_book_fav = if_else(!is.na(p_book), as.integer(p_book >= 0.5), NA_integer_),
        
        ATPk = k
      )
    
    all_preds[[length(all_preds) + 1]] <- preds
    
    # expanding pool update after each tournament
    train_pool <- bind_rows(train_pool, test_df)
  }
  
  all_df <- bind_rows(all_preds)
  
  comp_df <- all_df %>%
    filter(
      !is.na(B365W), !is.na(B365L),
      B365W > 0, B365L > 0,
      B365W != B365L,
      !is.na(ttt_correct),
      !is.na(bookie_correct),
      !is.na(p_ttt),
      !is.na(p_book)
    )
  
  inj_subset <- comp_df %>% filter(abs_w == 1L | abs_l == 1L)
  
  tibble(
    set_name      = set_name,
    n_tourneys    = length(ks),
    gap_days      = GAP_DAYS,
    alpha         = ALPHA,
    n_matches     = nrow(comp_df),
    
    ttt_acc       = round(mean(comp_df$ttt_correct) * 100, 2),
    book_acc      = round(mean(comp_df$bookie_correct) * 100, 2),
    
    brier_ttt     = round(brier_score(comp_df$p_ttt,  comp_df$y), 4),
    brier_book    = round(brier_score(comp_df$p_book, comp_df$y), 4),
    
    logloss_ttt   = round(log_loss(comp_df$p_ttt,  comp_df$y), 4),
    logloss_book  = round(log_loss(comp_df$p_book, comp_df$y), 4),
    
    calratio_ttt  = round(cal_ratio(comp_df$p_ttt_fav,  comp_df$y_ttt_fav), 4),
    calratio_book = round(cal_ratio(comp_df$p_book_fav, comp_df$y_book_fav), 4),
    
    inj_rate      = round(100 * mean(comp_df$abs_w == 1L | comp_df$abs_l == 1L), 2),
    n_inj         = nrow(inj_subset),
    ttt_acc_inj   = if (nrow(inj_subset) > 0) round(mean(inj_subset$ttt_correct) * 100, 2) else NA_real_,
    book_acc_inj  = if (nrow(inj_subset) > 0) round(mean(inj_subset$bookie_correct) * 100, 2) else NA_real_
  )
}

# ----------------------------
# Run multi-set + mini-grid
# ----------------------------
grid <- expand.grid(gap_days = GAP_GRID, alpha = ALPHA_GRID)

all_results <- list()
idx <- 1

for (sname in names(TOURNAMENT_SETS)) {
  
  atp_vec <- TOURNAMENT_SETS[[sname]]
  ks_set <- if (is.null(atp_vec)) ks_all else intersect(ks_all, atp_vec)
  
  cat("\n====================================================\n")
  cat(sprintf("Running TOURNAMENT SET: %s | n_tourneys=%d\n", sname, length(ks_set)))
  cat("ATP codes:\n")
  print(ks_set)
  cat("====================================================\n\n")
  
  for (i in seq_len(nrow(grid))) {
    gd <- grid$gap_days[i]
    a  <- grid$alpha[i]
    
    cat(sprintf("Set %s | combo %d/%d: GAP_DAYS=%d | ALPHA=%.3f\n",
                sname, i, nrow(grid), gd, a))
    
    all_results[[idx]] <- run_one_combo(
      GAP_DAYS = gd,
      ALPHA    = a,
      ks       = ks_set,
      set_name = sname
    )
    
    idx <- idx + 1
  }
}

final_tbl <- bind_rows(all_results) %>%
  arrange(desc(ttt_acc), set_name, gap_days, alpha)

cat("\n=== ALL TOURNAMENT-SET RESULTS (sorted by TTT accuracy) ===\n")
print(final_tbl, n = nrow(final_tbl))

write_csv(final_tbl, "ordinal_absence_multiset_results.csv")
cat("\nSaved: ordinal_absence_multiset_results.csv\n")
