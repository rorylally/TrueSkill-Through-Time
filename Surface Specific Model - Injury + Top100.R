#[All_2024_R1]
#TTT (from CSV skills)
#Accuracy — TTT: 60.71% | Book: 65.60% (same 593 matches)
#Brier — TTT: 0.2606 | Book: 0.2069
#LogLoss — TTT: 0.8013 | Book: 0.5958
#CalRatio — TTT: 0.5721 | Book: 0.5821

#[ATP250]
#TTT (from CSV skills)
#Accuracy — TTT: 53.63% | Book: 62.01% (same 179 matches)
#Brier — TTT: 0.3004 | Book: 0.2264
#LogLoss — TTT: 0.9313 | Book: 0.6428
#CalRatio — TTT: 0.5206 | Book: 0.5458

#[ATP500]
#TTT (from CSV skills)
#Accuracy — TTT: 63.93% | Book: 62.30% (same 122 matches)
#Brier — TTT: 0.2344 | Book: 0.2124
#LogLoss — TTT: 0.7112 | Book: 0.6118
#CalRatio — TTT: 0.5921 | Book: 0.5803

#[Masters1000]
#TTT (from CSV skills)
#Accuracy — TTT: 57.86% | Book: 64.29% (same 140 matches)
#Brier — TTT: 0.2828 | Book: 0.2208
#LogLoss — TTT: 0.8465 | Book: 0.6311
#CalRatio — TTT: 0.5360 | Book: 0.5573

#[Grandslams]
#TTT (from CSV skills)
#Accuracy — TTT: 72.73% | Book: 76.52% (same 132 matches)
#Brier — TTT: 0.1954 | Book: 0.1516
#LogLoss — TTT: 0.6356 | Book: 0.4596
#CalRatio — TTT: 0.6744 | Book: 0.6700


library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(tibble)

# ----------------------------
# 0) SETTINGS
# ----------------------------
MATCH_FILE <- "2023_2024_ATP_Results.xlsx"
PRED_FILE  <- "surface_ttt_preds_byATP.csv"

GAP_DAYS <- 16
ALPHA    <- 0.5
BETA     <- 1.0

# ----------------------------
# 1) Tournament sets
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

book_p_winner <- function(odds_w, odds_l) {
  iw <- 1 / odds_w
  il <- 1 / odds_l
  iw / (iw + il)
}

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
      p_ttt  = clip01(p_win_inj),
      p_book = clip01(book_p_winner(B365W, B365L))
    )
  
  if (nrow(comp) == 0) return(NULL)
  
  tibble(
    n = nrow(comp),
    ttt_acc  = mean(comp$ttt_correct_inj) * 100,
    book_acc = mean(comp$B365W < comp$B365L) * 100,
    ttt_brier  = mean((1 - comp$p_ttt)^2),
    book_brier = mean((1 - comp$p_book)^2),
    ttt_ll  = mean(-log(comp$p_ttt)),
    book_ll = mean(-log(comp$p_book)),
    ttt_cal  = mean(comp$p_ttt),
    book_cal = mean(comp$p_book)
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
# 3) Load base match data (FULL history for injuries)
# ----------------------------
df0 <- read_excel(MATCH_FILE) %>%
  mutate(
    Date   = to_date_safe(Date),
    Winner = trimws(Winner),
    Loser  = trimws(Loser),
    Round  = trimws(Round),
    ATP    = as.integer(ATP)
  )

# ----------------------------
# 4) Load surface preds CSV (TOP-100 FILTER HERE)
# ----------------------------
pred0 <- read_csv(PRED_FILE, show_col_types = FALSE) %>%
  mutate(
    Date   = to_date_safe(Date),
    Winner = trimws(Winner),
    Loser  = trimws(Loser),
    Round  = trimws(Round),
    ATP    = as.integer(ATP),
    ATPk   = as.integer(ATPk),
    
    WRank = as.numeric(WRank),
    LRank = as.numeric(LRank),
    
    mu_winner    = as.numeric(mu_winner),
    sigma_winner = as.numeric(sigma_winner),
    mu_loser     = as.numeric(mu_loser),
    sigma_loser  = as.numeric(sigma_loser),
    
    B365W = as.numeric(B365W),
    B365L = as.numeric(B365L)
  ) %>%
  # 🔑 TOP-100 ONLY
  filter(
    !is.na(WRank), !is.na(LRank),
    WRank <= 100, LRank <= 100
  )

# ----------------------------
# 5) Injury-adjust tournament by tournament
# ----------------------------
ks <- pred0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
  distinct(ATPk) %>%
  arrange(ATPk) %>%
  pull(ATPk)

train_pool <- df0 %>% filter(year(Date) == 2023)
all_out <- list()

for (k in ks) {
  
  test_pred <- pred0 %>%
    filter(year(Date) == 2024, ATPk == k, Round == "1st Round")
  if (nrow(test_pred) == 0) next
  
  anchor_date <- min(test_pred$Date)
  players_needed <- unique(c(test_pred$Winner, test_pred$Loser))
  
  abs_tbl <- build_absence_tbl(train_pool, players_needed, anchor_date, GAP_DAYS)
  
  out_k <- test_pred %>%
    left_join(abs_tbl, by = c("Winner" = "player")) %>%
    rename(inj_w = inj_flag) %>%
    left_join(abs_tbl, by = c("Loser" = "player")) %>%
    rename(inj_l = inj_flag) %>%
    mutate(
      inj_w = coalesce(inj_w, 0L),
      inj_l = coalesce(inj_l, 0L),
      
      mu_w_adj = mu_winner - ALPHA * sigma_winner * inj_w,
      mu_l_adj = mu_loser  - ALPHA * sigma_loser  * inj_l,
      
      z_inj = (mu_w_adj - mu_l_adj) /
        sqrt(2 * BETA^2 + sigma_winner^2 + sigma_loser^2),
      
      p_win_inj = pnorm(z_inj),
      pred_inj = if_else(p_win_inj >= 0.5, Winner, Loser),
      ttt_correct_inj = pred_inj == Winner
    )
  
  all_out[[length(all_out) + 1]] <- out_k
  
  train_pool <- bind_rows(
    train_pool,
    df0 %>% filter(year(Date) == 2024, ATP == k, Round == "1st Round")
  )
}

final_df <- bind_rows(all_out)

# ----------------------------
# 6) Tournament-split report (TOP-100)
# ----------------------------
for (nm in names(TOURNAMENT_SETS)) {
  
  atps <- TOURNAMENT_SETS[[nm]]
  
  df_sub <- if (is.null(atps)) {
    final_df
  } else {
    final_df %>% filter(ATPk %in% atps)
  }
  
  m <- metrics_block(df_sub)
  if (!is.null(m)) print_block(nm, m)
}
