#gap_days alpha n_matches ttt_acc book_acc inj_rate n_inj ttt_acc_inj book_acc_inj
#<dbl> <dbl>     <int>   <dbl>    <dbl>    <dbl> <int>       <dbl>        <dbl>
#  1       30  0.04       579    64.8     65.8     51.8   300        70.7         69 

# ==========================================================
# ORDINAL SNAPSHOT MODEL + ABSENCE ADJUSTMENT
# MINI FINE-TUNE GAP_DAYS (and optionally ALPHA) — SMALL GRID ONLY
# Evaluates 2024 1st Round, Top-100 completed matches
# Uses expanding train_pool and tournament anchor_date (same as your final model)
# Prints a results table at the end (no huge grid)
# ==========================================================

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)

# ----------------------------
# Hyperparams (prob calc)
# ----------------------------
beta <- 1

# ----------------------------
# MINI GRID (EDIT THESE ONLY)
# ----------------------------
GAP_GRID <- c(30)    # decreased gap focus
ALPHA_GRID <- c(0.04)           # keep small (2 values). set to c(0.5) if you only want alpha fixed

# ----------------------------
# Load & prep match data (Top-100 only)
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Comment = trimws(Comment),
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
skills_ord <- read_csv("skills_snapshot_ordinal.csv", show_col_types = FALSE) %>%
  mutate(
    player = trimws(player),
    ATP    = as.numeric(ATP),
    mu_generic = as.numeric(mu_generic),
    sigma_generic = as.numeric(sigma_generic)
  )

# ----------------------------
# Tournament list (same eval set)
# ----------------------------
ks <- df0 %>%
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
# Run one combo (GAP_DAYS, ALPHA)
# ----------------------------
run_one_combo <- function(GAP_DAYS, ALPHA) {
  
  all_preds  <- list()
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
        
        # adjustment
        mu_w_adj = mu_winner - ALPHA * sigma_winner * abs_w,
        mu_l_adj = mu_loser  - ALPHA * sigma_loser  * abs_l,
        
        denom = sqrt(pmax(2*beta^2 + sigma_winner^2 + sigma_loser^2, 1e-9)),
        z_val = (mu_w_adj - mu_l_adj) / denom,
        
        TTT_Prob_W = pnorm(z_val),
        pred        = if_else(TTT_Prob_W >= 0.5, Winner, Loser),
        ttt_correct = pred == Winner,
        
        bookie_pred = case_when(
          !is.na(B365W) & !is.na(B365L) & B365W < B365L ~ Winner,
          !is.na(B365W) & !is.na(B365L) & B365W > B365L ~ Loser,
          TRUE ~ NA_character_
        ),
        bookie_correct = !is.na(bookie_pred) & bookie_pred == Winner
      )
    
    all_preds[[length(all_preds) + 1]] <- preds
    train_pool <- bind_rows(train_pool, test_df)
  }
  
  all_df <- bind_rows(all_preds)
  
  comp_df <- all_df %>%
    filter(
      !is.na(B365W), !is.na(B365L),
      B365W > 0, B365L > 0,
      B365W != B365L,
      !is.na(ttt_correct),
      !is.na(bookie_correct)
    )
  
  inj_subset <- comp_df %>% filter(abs_w == 1L | abs_l == 1L)
  
  tibble(
    gap_days     = GAP_DAYS,
    alpha        = ALPHA,
    n_matches    = nrow(comp_df),
    ttt_acc      = round(mean(comp_df$ttt_correct) * 100, 2),
    book_acc     = round(mean(comp_df$bookie_correct) * 100, 2),
    inj_rate     = round(100 * mean(comp_df$abs_w == 1L | comp_df$abs_l == 1L), 2),
    n_inj        = nrow(inj_subset),
    ttt_acc_inj  = if (nrow(inj_subset) > 0) round(mean(inj_subset$ttt_correct) * 100, 2) else NA_real_,
    book_acc_inj = if (nrow(inj_subset) > 0) round(mean(inj_subset$bookie_correct) * 100, 2) else NA_real_
  )
}

# ----------------------------
# Run mini grid
# ----------------------------
grid <- expand.grid(gap_days = GAP_GRID, alpha = ALPHA_GRID)

res_list <- vector("list", nrow(grid))

for (i in seq_len(nrow(grid))) {
  gd <- grid$gap_days[i]
  a  <- grid$alpha[i]
  cat(sprintf("Running %d/%d: GAP_DAYS=%d | ALPHA=%.2f\n", i, nrow(grid), gd, a))
  res_list[[i]] <- run_one_combo(GAP_DAYS = gd, ALPHA = a)
}

res_tbl <- bind_rows(res_list) %>%
  arrange(desc(ttt_acc), gap_days, alpha)

cat("\n=== MINI GRID RESULTS (sorted by TTT accuracy) ===\n")
print(res_tbl, n = nrow(res_tbl))



