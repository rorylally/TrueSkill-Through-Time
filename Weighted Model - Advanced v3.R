#[All_2024]
#ORDINAL TTT 
#Accuracy — TTT: 65.08% | Book: 69.11% (same 2609 matches)
#Brier — TTT: 0.2201 | Book: 0.1990
#LogLoss — TTT: 0.6311 | Book: 0.5810
#CalRatio — TTT: 0.5500 | Book: 0.5957

#[ATP250]                                                                                                                     
#ORDINAL TTT 
#Accuracy — TTT: 62.34% | Book: 66.12% (same 1033 matches)
#Brier — TTT: 0.2333 | Book: 0.2141
#LogLoss — TTT: 0.6595 | Book: 0.6166
#CalRatio — TTT: 0.5326 | Book: 0.5662

#[ATP500]                                                                                                                     
#ORDINAL TTT 
#Accuracy — TTT: 62.89% | Book: 67.53% (same 388 matches)
#Brier — TTT: 0.2210 | Book: 0.2018
#LogLoss — TTT: 0.6325 | Book: 0.5875
#CalRatio — TTT: 0.5478 | Book: 0.5902

#[Masters1000]                                                                                                                
#ORDINAL TTT 
#Accuracy — TTT: 65.11% | Book: 67.85% (same 622 matches)
#Brier — TTT: 0.2174 | Book: 0.2056
#LogLoss — TTT: 0.6255 | Book: 0.5967
#CalRatio — TTT: 0.5534 | Book: 0.5898

#[Grandslams]                                                                                                                 
#ORDINAL TTT 
#Accuracy — TTT: 73.00% | Book: 78.40% (same 500 matches)
#Brier — TTT: 0.1940 | Book: 0.1539
#LogLoss — TTT: 0.5754 | Book: 0.4731
#CalRatio — TTT: 0.5854 | Book: 0.6719



library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(tibble)
library(writexl)

# ----------------------------
# Hyperparams (used ONLY in prob calc)
# ----------------------------
beta <- 1

# ----------------------------
# Tournament sets
# ----------------------------
TOURNAMENT_SETS <- list(
  All_2024 = NULL,
  ATP250 = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27, 30, 31, 33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53, 57, 58, 59, 63, 64),
  ATP500 = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60, 61),
  Masters1000 = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams = c(5, 32, 39, 51)
)

# ----------------------------
# Proper scoring rules
# ----------------------------
brier_score <- function(p, y) mean((p - y)^2, na.rm = TRUE)

log_loss <- function(p, y) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
}

# ----------------------------
# Load & prep match data (Top-100 only)
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Year    = year(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Round   = trimws(Round),
    Comment = trimws(Comment),
    WRank   = suppressWarnings(as.numeric(WRank)),
    LRank   = suppressWarnings(as.numeric(LRank)),
    Wsets   = as.integer(Wsets),
    Lsets   = as.integer(Lsets)
  ) %>%
  mutate(
    margin = pmin(pmax(Wsets - Lsets, 1), 3)
  )

skills_ord <- read_csv("skills_snapshot_ordinal2v2.csv", show_col_types = FALSE) %>%
  mutate(player = trimws(player))


ks_all <- df0 %>%
  filter(Year == 2024) %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ============================
# Runner for one tournament set
# ============================
run_ordinal_for_set <- function(set_name, atp_vec_or_null) {
  
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
    
    # Snapshot skills at tournament k
    stats_wide <- skills_ord %>%
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
        
        # Ordinal-consistent win probability
        denom = sqrt(pmax(2*beta^2 + sigma_winner^2 + sigma_loser^2, 1e-9)),
        z_val = (mu_winner - mu_loser) / denom,
        
        p_win_ttt  = pnorm(z_val),
        p_lose_ttt = 1 - p_win_ttt,
        
        pred        = if_else(p_win_ttt >= 0.5, Winner, Loser),
        ttt_correct = pred == Winner,
        
        # Bet365 hard pred + normalized implied prob (remove overround)
        bookie_pred = case_when(
          !is.na(B365W) & !is.na(B365L) & B365W < B365L ~ Winner,
          !is.na(B365W) & !is.na(B365L) & B365W > B365L ~ Loser,
          TRUE ~ NA_character_
        ),
        bookie_correct = !is.na(bookie_pred) & bookie_pred == Winner,
        
        p_win_book = if_else(
          !is.na(B365W) & !is.na(B365L) & B365W > 0 & B365L > 0 & B365W != B365L,
          (1 / B365W) / ((1 / B365W) + (1 / B365L)),
          NA_real_
        ),
        p_lose_book = if_else(!is.na(p_win_book), 1 - p_win_book, NA_real_),
        
        actual = 1,
        ATPk = k,
        tournament_set = set_name
      )
    
    for (j in seq_len(nrow(preds))) {
      match_list[[ctr]] <- preds[j, ] %>%
        transmute(
          Date,
          ATP,
          Tournament = if ("Tournament" %in% names(df0)) as.character(Tournament) else NA_character_,
          Round,
          Winner,
          Loser,
          WRank, LRank,
          Wsets, Lsets,
          margin,
          
          mu_winner, mu_loser,
          sigma_winner, sigma_loser,
          denom, z_val,
          
          p_win_ttt, p_lose_ttt,
          p_win_book, p_lose_book,
          
          ttt_correct, bookie_correct,
          actual,
          tournament_set,
          ATPk,
          B365W, B365L
        )
      ctr <- ctr + 1L
    }
  }
  
  all_df <- bind_rows(match_list)
  
  # Evaluate on same matches: require usable odds & probs (same style as your Elo/TTT outputs)
  comp_df <- all_df %>%
    filter(
      !is.na(B365W), !is.na(B365L),
      B365W > 0, B365L > 0,
      B365W != B365L,
      !is.na(p_win_book),
      !is.na(p_win_ttt),
      !is.na(ttt_correct),
      !is.na(bookie_correct)
    )
  
  n_comp <- nrow(comp_df)
  
  # Accuracy
  acc_ttt  <- mean((comp_df$p_win_ttt >= 0.5) == (comp_df$actual == 1))
  acc_book <- mean((comp_df$p_win_book >= 0.5) == (comp_df$actual == 1))
  
  # Brier
  brier_ttt  <- brier_score(comp_df$p_win_ttt, comp_df$actual)
  brier_book <- brier_score(comp_df$p_win_book, comp_df$actual)
  
  # LogLoss
  ll_ttt  <- log_loss(comp_df$p_win_ttt, comp_df$actual)
  ll_book <- log_loss(comp_df$p_win_book, comp_df$actual)
  
  # CalRatio (here equals mean(p_win) because actual=1 for all rows)
  cal_ttt  <- sum(comp_df$p_win_ttt) / sum(comp_df$actual)
  cal_book <- sum(comp_df$p_win_book) / sum(comp_df$actual)
  
  cat(sprintf(
    "[%s]\nORDINAL TTT \nAccuracy — TTT: %.2f%% | Book: %.2f%% (same %d matches)\nBrier — TTT: %.4f | Book: %.4f\nLogLoss — TTT: %.4f | Book: %.4f\nCalRatio — TTT: %.4f | Book: %.4f\n\n",
    set_name,
    100*acc_ttt, 100*acc_book, n_comp,
    brier_ttt, brier_book,
    ll_ttt, ll_book,
    cal_ttt, cal_book
  ))
  
  # Exports per set
  preds_path   <- sprintf("Ordinal_TTT_predictions_TOP100_%s.csv", set_name)
  summary_path <- sprintf("metrics_summary_ORDINAL_TTT_TOP100_%s.csv", set_name)
  xlsx_path    <- sprintf("Global_TTT_ordinal_predictions_TOP100_FROMCSV_%s.xlsx", set_name)
  
  write_csv(all_df, preds_path)
  
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
    
    preds_file = preds_path,
    xlsx_file  = xlsx_path
  )
  write_csv(summary_df, summary_path)
  
  write_xlsx(all_df, xlsx_path)
  
  list(
    tournament_set = set_name,
    n_tournaments_used = length(ks),
    n_matches = n_comp,
    acc_ttt = 100*acc_ttt,
    acc_book = 100*acc_book,
    brier_ttt = brier_ttt,
    brier_book = brier_book,
    logloss_ttt = ll_ttt,
    logloss_book = ll_book,
    calratio_ttt = cal_ttt,
    calratio_book = cal_book,
    preds_file = preds_path,
    summary_file = summary_path,
    xlsx_file = xlsx_path
  )
}

# ============================
# Run all sets + combined summary
# ============================
results <- lapply(names(TOURNAMENT_SETS), function(nm) {
  run_ordinal_for_set(nm, TOURNAMENT_SETS[[nm]])
})

results_df <- bind_rows(lapply(results, function(x) {
  tibble(
    tournament_set     = x$tournament_set,
    n_tournaments_used = x$n_tournaments_used,
    n_matches          = x$n_matches,
    
    acc_ttt            = round(x$acc_ttt, 2),
    acc_book           = round(x$acc_book, 2),
    
    brier_ttt          = round(x$brier_ttt, 4),
    brier_book         = round(x$brier_book, 4),
    
    logloss_ttt        = round(x$logloss_ttt, 4),
    logloss_book       = round(x$logloss_book, 4),
    
    calratio_ttt       = round(x$calratio_ttt, 4),
    calratio_book      = round(x$calratio_book, 4),
    
    preds_file         = x$preds_file,
    summary_file       = x$summary_file,
    xlsx_file          = x$xlsx_file
  )
})) %>%
  arrange(desc(acc_ttt))


