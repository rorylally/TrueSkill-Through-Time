#[All_2024_R1]
#TTT (from CSV skills)
#Accuracy — TTT: 64.85% | Book: 69.11% (same 2609 matches)
#Brier — TTT: 0.2250 | Book: 0.1990
#LogLoss — TTT: 0.6529 | Book: 0.5810
#CalRatio — TTT: 0.5805 | Book: 0.5957

#[ATP250]                                                                                                                          
#TTT (from CSV skills)
#Accuracy — TTT: 61.08% | Book: 66.12% (same 1033 matches)
#Brier — TTT: 0.2445 | Book: 0.2141
#LogLoss — TTT: 0.6971 | Book: 0.6166
#CalRatio — TTT: 0.5536 | Book: 0.5662

#[ATP500]                                                                                                                          
#TTT (from CSV skills)
#Accuracy — TTT: 64.18% | Book: 67.53% (same 388 matches)
#Brier — TTT: 0.2204 | Book: 0.2018
#LogLoss — TTT: 0.6409 | Book: 0.5875
#CalRatio — TTT: 0.5789 | Book: 0.5902

#[Masters1000]                                                                                                                     
#TTT (from CSV skills)
#Accuracy — TTT: 65.59% | Book: 67.85% (same 622 matches)
#Brier — TTT: 0.2215 | Book: 0.2056
#LogLoss — TTT: 0.6471 | Book: 0.5967
#CalRatio — TTT: 0.5857 | Book: 0.5898

#[Grandslams]                                                                                                                      
#TTT (from CSV skills)
#Accuracy — TTT: 72.80% | Book: 78.40% (same 500 matches)
#Brier — TTT: 0.1894 | Book: 0.1539
#LogLoss — TTT: 0.5722 | Book: 0.4731
#CalRatio — TTT: 0.6349 | Book: 0.6719

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
  
  # Same-match evaluation: require usable odds probability (like your Elo script style)
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
  
  # Metrics: TTT
  acc_ttt   <- mean((comp$p_win_ttt >= 0.5) == (comp$actual == 1))
  brier_ttt <- brier_score(comp$p_win_ttt, comp$actual)
  ll_ttt    <- log_loss(comp$p_win_ttt, comp$actual)
  cal_ttt   <- sum(comp$p_win_ttt) / sum(comp$actual)
  
  # Metrics: Book
  acc_book   <- mean((comp$p_win_book >= 0.5) == (comp$actual == 1))
  brier_book <- brier_score(comp$p_win_book, comp$actual)
  ll_book    <- log_loss(comp$p_win_book, comp$actual)
  cal_book   <- sum(comp$p_win_book) / sum(comp$actual)
  
  cat(sprintf(
    "[%s]\nTTT (from CSV skills)\nAccuracy — TTT: %.2f%% | Book: %.2f%% (same %d matches)\nBrier — TTT: %.4f | Book: %.4f\nLogLoss — TTT: %.4f | Book: %.4f\nCalRatio — TTT: %.4f | Book: %.4f\n\n",
    set_name,
    100*acc_ttt, 100*acc_book, n_comp,
    brier_ttt, brier_book,
    ll_ttt, ll_book,
    cal_ttt, cal_book
  ))
  
  # Exports per set (match-level predictions + set summary)
  preds_path   <- sprintf("TTT_predictions_%s.csv", set_name)
  summary_path <- sprintf("metrics_summary_TTT_%s.csv", set_name)
  
  write_csv(all_preds, preds_path)
  
  summary_df <- tibble(
    tournament_set = set_name,
    n_tournaments_used = length(ks),
    n_matches = n_comp,
    
    acc_ttt  = round(100*acc_ttt, 2),
    acc_book = round(100*acc_book, 2),
    
    brier_ttt  = round(brier_ttt, 4),
    brier_book = round(brier_book, 4),
    
    logloss_ttt  = round(ll_ttt, 4),
    logloss_book = round(ll_book, 4),
    
    calratio_ttt  = round(cal_ttt, 4),
    calratio_book = round(cal_book, 4),
    
    preds_file = preds_path
  )
  write_csv(summary_df, summary_path)
  
  list(
    set_name = set_name,
    ks = ks,
    n_comp = n_comp,
    acc_ttt = 100*acc_ttt,
    acc_book = 100*acc_book,
    brier_ttt = brier_ttt,
    brier_book = brier_book,
    logloss_ttt = ll_ttt,
    logloss_book = ll_book,
    calratio_ttt = cal_ttt,
    calratio_book = cal_book,
    preds_path = preds_path,
    summary_path = summary_path
  )
}

# ----------------------------
# 5) Run all sets + combined summary
# ----------------------------
results <- lapply(names(TOURNAMENT_SETS), function(nm) {
  run_ttt_for_set(nm, TOURNAMENT_SETS[[nm]])
})

results_df <- bind_rows(lapply(results, function(x) {
  tibble(
    tournament_set = x$set_name,
    n_tournaments_used = length(x$ks),
    n_matches = x$n_comp,
    
    acc_ttt = round(x$acc_ttt, 2),
    acc_book = round(x$acc_book, 2),
    
    brier_ttt = round(x$brier_ttt, 4),
    brier_book = round(x$brier_book, 4),
    
    logloss_ttt = round(x$logloss_ttt, 4),
    logloss_book = round(x$logloss_book, 4),
    
    calratio_ttt = round(x$calratio_ttt, 4),
    calratio_book = round(x$calratio_book, 4),
    
    preds_file = x$preds_path,
    summary_file = x$summary_path
  )
})) %>%
  arrange(desc(acc_ttt))

