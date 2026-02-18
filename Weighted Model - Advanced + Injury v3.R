#tournament_set gap_days alpha n_matches acc_ttt acc_book brier_ttt brier_book logloss_ttt logloss_book
#<chr>             <dbl> <dbl>     <int>   <dbl>    <dbl>     <dbl>      <dbl>       <dbl>        <dbl>
#1 Grandslams           16   0.5       478    73.2     78.5     0.192      0.155       0.571        0.475
#2 Masters1000          16   0.5       608    67.1     68.6     0.215      0.202       0.62         0.588
#3 All_2024_R1          16   0.5      2516    65.6     69.6     0.219      0.197       0.63         0.577
#4 ATP500               16   0.5       363    65       67.5     0.217      0.199       0.623        0.581
#5 ATP250               16   0.5      1004    62.2     67       0.233      0.211       0.661        0.611


library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)

beta <- 1
GAP_GRID   <- c(16)
ALPHA_GRID <- c(0.5)

TOURNAMENT_SETS <- list(
  All_2024_R1 = NULL,
  ATP250      = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27, 30, 31, 33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53, 57, 58, 59, 63, 64),
  ATP500      = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60, 61),
  Masters1000 = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams  = c(5, 32, 39, 51)
)

# ----------------------------
# Metrics helpers
# ----------------------------
brier_score <- function(p, y) mean((p - y)^2, na.rm = TRUE)

log_loss <- function(p, y) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
}

# Kovalchik-style global calibration ratio for "favorite" predictions:
# cal = sum(p_fav) / sum(1{fav_wins})
cal_ratio_fav <- function(p_win, y_win = 1L) {
  # p_win is prob that Winner wins (so y_win is typically 1 for each row here)
  p_fav <- pmax(p_win, 1 - p_win)
  fav_wins <- ifelse(p_win >= 0.5, y_win, 1 - y_win)
  denom <- sum(fav_wins, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  sum(p_fav, na.rm = TRUE) / denom
}

# ----------------------------
# Load match data
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Comment = trimws(Comment),
    
    WRank = suppressWarnings(as.numeric(WRank)),
    LRank = suppressWarnings(as.numeric(LRank)),
    
    Wsets = suppressWarnings(as.integer(Wsets)),
    Lsets = suppressWarnings(as.integer(Lsets)),
    
    # ensure odds numeric
    B365W = suppressWarnings(as.numeric(B365W)),
    B365L = suppressWarnings(as.numeric(B365L))
  ) %>%
  filter(
    Comment == "Completed"
  ) %>%
  mutate(
    margin = pmin(pmax(Wsets - Lsets, 1), 3)
  )

# ----------------------------
# Load ordinal skill snapshots
# Expected columns: ATP | player | mu_generic | sigma_generic
# ----------------------------
skills_ord <- read_csv("skills_snapshot_ordinal2.csv", show_col_types = FALSE) %>%
  mutate(
    player = trimws(player),
    ATP    = as.numeric(ATP),
    mu_generic    = as.numeric(mu_generic),
    sigma_generic = as.numeric(sigma_generic)
  )

# ----------------------------
# All tournaments available in eval set
# ----------------------------
ks_all <- df0 %>%
  filter(year(Date) == 2024) %>%
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
# Returns ONE summary row (like your example)
# ----------------------------
run_one_combo <- function(GAP_DAYS, ALPHA, ks, set_name) {
  
  all_preds  <- list()
  
  # Seed expanding pool with 2023 matches
  train_pool <- df0 %>% filter(year(Date) == 2023)
  
  for (k in ks) {
    
    test_df <- df0 %>% filter(year(Date) == 2024, ATP == k)
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
        
        # ordinal-consistent probability: P(Winner wins)
        denom = sqrt(pmax(2*beta^2 + sigma_winner^2 + sigma_loser^2, 1e-9)),
        z_val = (mu_w_adj - mu_l_adj) / denom,
        TTT_Prob_W = pnorm(z_val),
        
        # bookmaker implied prob for Winner (properly normalized)
        book_prob_w = if_else(
          !is.na(B365W) & !is.na(B365L) & B365W > 0 & B365L > 0 & B365W != B365L,
          (1 / B365W) / ((1 / B365W) + (1 / B365L)),
          NA_real_
        ),
        
        ATPk = k
      )
    
    all_preds[[length(all_preds) + 1]] <- preds
    
    # expanding pool update
    train_pool <- bind_rows(train_pool, test_df)
  }
  
  all_df <- bind_rows(all_preds)
  
  # keep only rows where BOTH probs exist + bookmaker odds valid
  comp_df <- all_df %>%
    filter(
      !is.na(TTT_Prob_W),
      is.finite(TTT_Prob_W),
      !is.na(book_prob_w),
      is.finite(book_prob_w)
    )
  
  # If no comparable matches, return NA metrics
  if (nrow(comp_df) == 0) {
    return(tibble(
      tournament_set       = set_name,
      n_tournaments_used   = length(ks),
      gap_days             = GAP_DAYS,
      alpha                = ALPHA,
      n_matches            = 0L,
      acc_ttt              = NA_real_,
      acc_book             = NA_real_,
      brier_ttt            = NA_real_,
      brier_book           = NA_real_,
      logloss_ttt          = NA_real_,
      logloss_book         = NA_real_,
      calratio_ttt         = NA_real_
    ))
  }
  
  # Outcome is always Winner in your dataset rows => y=1
  y <- rep(1, nrow(comp_df))
  
  ttt_correct  <- (comp_df$TTT_Prob_W  >= 0.5)
  book_correct <- (comp_df$book_prob_w >= 0.5)
  
  tibble(
    tournament_set       = set_name,
    gap_days             = GAP_DAYS,
    alpha                = ALPHA,
    n_matches            = nrow(comp_df),
    
    acc_ttt              = round(mean(ttt_correct,  na.rm = TRUE) * 100, 1),
    acc_book             = round(mean(book_correct, na.rm = TRUE) * 100, 1),
    
    brier_ttt            = round(brier_score(comp_df$TTT_Prob_W,  y), 3),
    brier_book           = round(brier_score(comp_df$book_prob_w, y), 3),
    
    logloss_ttt          = round(log_loss(comp_df$TTT_Prob_W,  y), 3),
    logloss_book         = round(log_loss(comp_df$book_prob_w, y), 3),
    
    calratio_ttt         = round(cal_ratio_fav(comp_df$TTT_Prob_W, y_win = 1L), 3)
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
  arrange(desc(acc_ttt), tournament_set, gap_days, alpha)

cat("\n=== RESULTS (sorted by TTT accuracy) ===\n")
print(final_tbl, n = nrow(final_tbl))

write_csv(final_tbl, "ordinal_absence_multiset_results_fullmetrics.csv")
cat("\nSaved: ordinal_absence_multiset_results_fullmetrics.csv\n")
