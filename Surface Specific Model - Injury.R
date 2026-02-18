#[All_2024_R1]
#TTT (from CSV skills)
#Accuracy — TTT: 64.12% | Book: 69.11% (same 2609 matches)
#Brier — TTT: 0.2424 | Book: 0.1990
#LogLoss — TTT: 0.7478 | Book: 0.5810
#CalRatio — TTT: 0.5940 | Book: 0.5957

#[ATP250]
#TTT (from CSV skills)
#Accuracy — TTT: 60.70% | Book: 66.12% (same 1033 matches)
#Brier — TTT: 0.2685 | Book: 0.2141
#LogLoss — TTT: 0.8429 | Book: 0.6166
#CalRatio — TTT: 0.5637 | Book: 0.5662

#[ATP500]
#TTT (from CSV skills)
#Accuracy — TTT: 64.69% | Book: 67.53% (same 388 matches)
#Brier — TTT: 0.2383 | Book: 0.2018
#LogLoss — TTT: 0.7243 | Book: 0.5875
#CalRatio — TTT: 0.5937 | Book: 0.5902

#[Masters1000]
#TTT (from CSV skills)
#Accuracy — TTT: 63.34% | Book: 67.85% (same 622 matches)
#Brier — TTT: 0.2358 | Book: 0.2056
#LogLoss — TTT: 0.7069 | Book: 0.5967
#CalRatio — TTT: 0.5965 | Book: 0.5898

#[Grandslams]
#TTT (from CSV skills)
#Accuracy — TTT: 73.00% | Book: 78.40% (same 500 matches)
#Brier — TTT: 0.1977 | Book: 0.1539
#LogLoss — TTT: 0.6206 | Book: 0.4731
#CalRatio — TTT: 0.6572 | Book: 0.6719

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(tibble)

# ----------------------------
# 0) SETTINGS (EDIT THESE)
# ----------------------------
MATCH_FILE <- "2023_2024_ATP_Results.xlsx"
PRED_FILE  <- "surface_ttt_preds_byATPv2.csv"   # your match-level surface preds CSV

GAP_DAYS <- 12
ALPHA    <- 0.16
BETA     <- 1.0

# ----------------------------
# 1) Tournament sets (your previous split)
# ----------------------------
TOURNAMENT_SETS <- list(
  All_2024_R1 = NULL,
  ATP250 = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27,
             30, 31, 33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53,
             57, 58, 59, 63, 64),
  ATP500 = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60, 61),
  Masters1000 = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams = c(5, 32, 39, 51)
)

# ----------------------------
# 2) Helpers
# ----------------------------
to_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct","POSIXlt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  as.Date(as.character(x))
}

clip01 <- function(p, eps = 1e-15) pmin(pmax(p, eps), 1 - eps)

# Book implied probability of the ACTUAL winner (vig-adjusted)
book_p_winner <- function(odds_w, odds_l) {
  iw <- 1 / odds_w
  il <- 1 / odds_l
  iw / (iw + il)
}

# Build absence flags from expanding train_pool
build_absence_tbl <- function(train_pool, players_needed, anchor_date, GAP_DAYS) {
  
  last_played <- train_pool %>%
    select(Date, Winner, Loser) %>%
    pivot_longer(cols = c(Winner, Loser),
                 names_to = "role", values_to = "player") %>%
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

# Metric block for one subset (uses injury-adjusted p_win_inj)
metrics_block <- function(df) {
  
  comp <- df %>%
    filter(
      !is.na(B365W), !is.na(B365L),
      B365W > 0, B365L > 0,
      B365W != B365L,
      !is.na(p_win_inj),
      !is.na(ttt_correct_inj)
    ) %>%
    mutate(
      p_ttt  = clip01(as.numeric(p_win_inj)),
      p_book = clip01(book_p_winner(B365W, B365L))
    )
  
  if (nrow(comp) == 0) return(NULL)
  
  tibble(
    n = nrow(comp),
    
    # Accuracy: TTT uses injury-adjusted correctness
    ttt_acc  = mean(comp$ttt_correct_inj, na.rm = TRUE) * 100,
    # Book accuracy: simply which side has lower odds
    book_acc = mean(comp$B365W < comp$B365L, na.rm = TRUE) * 100,
    
    # Since each row is an ACTUAL winner-row, y=1:
    # Brier = (1 - p)^2, LogLoss = -log(p), CalRatio = mean(p)
    ttt_brier  = mean((1 - comp$p_ttt)^2,  na.rm = TRUE),
    book_brier = mean((1 - comp$p_book)^2, na.rm = TRUE),
    
    ttt_ll  = mean(-log(comp$p_ttt),  na.rm = TRUE),
    book_ll = mean(-log(comp$p_book), na.rm = TRUE),
    
    ttt_cal  = mean(comp$p_ttt,  na.rm = TRUE),
    book_cal = mean(comp$p_book, na.rm = TRUE)
  )
}

print_block <- function(name, m) {
  cat(sprintf("#[%s]\n", name))
  cat("TTT (from CSV skills)\n")
  cat(sprintf("Accuracy — TTT: %.2f%% | Book: %.2f%% (same %d matches)\n",
              m$ttt_acc, m$book_acc, m$n))
  cat(sprintf("Brier — TTT: %.4f | Book: %.4f\n",
              m$ttt_brier, m$book_brier))
  cat(sprintf("LogLoss — TTT: %.4f | Book: %.4f\n",
              m$ttt_ll, m$book_ll))
  cat(sprintf("CalRatio — TTT: %.4f | Book: %.4f\n\n",
              m$ttt_cal, m$book_cal))
}

# ----------------------------
# 3) Load base match data (for train_pool)
# ----------------------------
df0 <- read_excel(MATCH_FILE) %>%
  mutate(
    Date   = to_date_safe(Date),
    Winner = trimws(Winner),
    Loser  = trimws(Loser),
    Round  = trimws(Round),
    ATP    = suppressWarnings(as.integer(ATP))
    
  )

# ----------------------------
# 4) Load surface preds CSV (match-level; already contains mu/sigma)
# ----------------------------
pred0 <- read_csv(PRED_FILE, show_col_types = FALSE) %>%
  mutate(
    Date   = to_date_safe(Date),
    Winner = trimws(Winner),
    Loser  = trimws(Loser),
    Round  = trimws(Round),
    
    ATP    = suppressWarnings(as.integer(ATP)),
    ATPk   = suppressWarnings(as.integer(ATPk)),
    
    WRank = as.numeric(WRank),
    LRank = as.numeric(LRank),
    
    mu_winner    = suppressWarnings(as.numeric(mu_winner)),
    sigma_winner = suppressWarnings(as.numeric(sigma_winner)),
    mu_loser     = suppressWarnings(as.numeric(mu_loser)),
    sigma_loser  = suppressWarnings(as.numeric(sigma_loser)),
    
    B365W = suppressWarnings(as.numeric(B365W)),
    B365L = suppressWarnings(as.numeric(B365L)),
)
# If ATPk is missing, fall back to ATP
if (!("ATPk" %in% names(pred0))) {
  pred0 <- pred0 %>% mutate(ATPk = ATP)
}

# tournaments to evaluate (2024 1st round)
ks <- pred0 %>%
  filter(year(Date) == 2024) %>%
  distinct(ATPk) %>%
  arrange(ATPk) %>%
  pull(ATPk)

# ----------------------------
# 5) Injury-adjust tournament-by-tournament (expanding train_pool)
# ----------------------------
train_pool <- df0 %>% filter(year(Date) == 2023)
all_out <- list()

for (k in ks) {
  
  test_pred <- pred0 %>%
    filter(year(Date) == 2024, ATPk == k)
  if (nrow(test_pred) == 0) next
  
  anchor_date <- min(test_pred$Date, na.rm = TRUE)
  players_needed <- unique(c(test_pred$Winner, test_pred$Loser))
  
  abs_tbl <- build_absence_tbl(
    train_pool     = train_pool,
    players_needed = players_needed,
    anchor_date    = anchor_date,
    GAP_DAYS       = GAP_DAYS
  )
  
  out_k <- test_pred %>%
    left_join(abs_tbl %>% select(player, inj_flag, days_since_play),
              by = c("Winner" = "player")) %>%
    rename(inj_w = inj_flag, days_w = days_since_play) %>%
    left_join(abs_tbl %>% select(player, inj_flag, days_since_play),
              by = c("Loser" = "player")) %>%
    rename(inj_l = inj_flag, days_l = days_since_play) %>%
    mutate(
      inj_w = as.integer(coalesce(inj_w, 0L)),
      inj_l = as.integer(coalesce(inj_l, 0L)),
      
      # SAME injury adjustment you used
      mu_w_adj = mu_winner - ALPHA * sigma_winner * inj_w,
      mu_l_adj = mu_loser  - ALPHA * sigma_loser  * inj_l,
      
      z_inj = (mu_w_adj - mu_l_adj) / sqrt(2 * BETA^2 + sigma_winner^2 + sigma_loser^2),
      p_win_inj = pnorm(z_inj),
      
      pred_inj = if_else(p_win_inj >= 0.5, Winner, Loser),
      ttt_correct_inj = (pred_inj == Winner)
    )
  
  all_out[[length(all_out) + 1]] <- out_k
  
  # expand train_pool with actual matches for that tournament
  new_matches <- df0 %>%
    filter(year(Date) == 2024, ATP == k)
  train_pool <- bind_rows(train_pool, new_matches)
}

all_df <- bind_rows(all_out)

# ----------------------------
# 6) Print tournament split report (injury-adjusted)
# ----------------------------
final_df <- all_df %>%
  filter(year(Date) == 2024)

for (nm in names(TOURNAMENT_SETS)) {
  
  atps <- TOURNAMENT_SETS[[nm]]
  
  df_sub <- if (is.null(atps)) {
    final_df
  } else {
    final_df %>% filter(ATPk %in% atps)
  }
  
  m <- metrics_block(df_sub)
  if (is.null(m)) next
  
  print_block(nm, m)
}

