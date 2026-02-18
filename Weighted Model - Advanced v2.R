#[All_2024_R1] Accuracy — ORDINAL TTT (Top-100, CSV): 64.25% | Bet365: 65.80% (same 579 matches)
#[ATP250] Accuracy — ORDINAL TTT (Top-100, CSV): 61.24% | Bet365: 62.36% (same 178 matches)                   
#[ATP500] Accuracy — ORDINAL TTT (Top-100, CSV): 60.68% | Bet365: 62.39% (same 117 matches)                   
#[Masters1000] Accuracy — ORDINAL TTT (Top-100, CSV): 62.77% | Bet365: 64.23% (same 137 matches)              
#[Grandslams] Accuracy — ORDINAL TTT (Top-100, CSV): 75.59% | Bet365: 77.17% (same 127 matches)  

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(writexl)

# ----------------------------
# Hyperparams (used ONLY in prob calc)
# ----------------------------
beta <- 1

# ----------------------------
# Tournament sets
# ----------------------------
TOURNAMENT_SETS <- list(
  All_2024_R1 = NULL,
  ATP250 = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27, 30, 31, 33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53, 57, 58, 59, 63, 64),
  ATP500 = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60, 61),
  Masters1000 = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams = c(5, 32, 39, 51)
)

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
  mutate(player = trimws(player))

# ----------------------------
# Base tournament list present in 2024 R1 (Top-100 only)
# ----------------------------
ks_all <- df0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
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
  
  all_preds <- list()
  
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(year(Date) == 2024, ATP == k, Round == "1st Round")
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
        
        TTT_Prob_W = pnorm(z_val),
        TTT_Prob_L = 1 - TTT_Prob_W,
        
        pred        = if_else(TTT_Prob_W >= 0.5, Winner, Loser),
        ttt_correct = pred == Winner,
        
        # Bookmakers
        bookie_pred = case_when(
          !is.na(B365W) & !is.na(B365L) & B365W < B365L ~ Winner,
          !is.na(B365W) & !is.na(B365L) & B365W > B365L ~ Loser,
          TRUE ~ NA_character_
        ),
        bookie_correct = !is.na(bookie_pred) & bookie_pred == Winner,
        
        ATPk = k,
        tournament_set = set_name
      )
    
    all_preds[[length(all_preds) + 1]] <- preds
  }
  
  all_df <- bind_rows(all_preds)
  
  # Evaluation (same matches)
  comp_df <- all_df %>%
    filter(
      !is.na(B365W), !is.na(B365L),
      B365W > 0, B365L > 0,
      B365W != B365L
    )
  
  ttt_acc  <- mean(comp_df$ttt_correct, na.rm = TRUE) * 100
  book_acc <- mean(comp_df$bookie_correct, na.rm = TRUE) * 100
  n_comp   <- nrow(comp_df)
  
  cat(sprintf(
    "[%s] Accuracy — ORDINAL TTT (Top-100, CSV): %.2f%% | Bet365: %.2f%% (same %d matches)\n",
    set_name, ttt_acc, book_acc, n_comp
  ))
  
  # Exports per set
  disagree_path <- sprintf("bet365_right_ttt_wrong_ordinal_TOP100_FROMCSV_%s.csv", set_name)
  summary_path  <- sprintf("accuracy_summary_ordinal_TOP100_FROMCSV_%s.csv", set_name)
  xlsx_path     <- sprintf("Global_TTT_ordinal_predictions_TOP100_FROMCSV_%s.xlsx", set_name)
  
  write_csv(
    comp_df %>% filter(bookie_correct & !ttt_correct),
    disagree_path
  )
  
  write_csv(
    tibble(
      tournament_set = set_name,
      ttt_accuracy = round(ttt_acc, 2),
      bookmaker_accuracy = round(book_acc, 2),
      n_matches = n_comp,
      n_tournaments_used = length(ks)
    ),
    summary_path
  )
  
  write_xlsx(
    all_df,
    xlsx_path
  )
  
  list(
    tournament_set = set_name,
    n_tournaments_used = length(ks),
    n_matches = n_comp,
    ttt_accuracy = ttt_acc,
    bookmaker_accuracy = book_acc,
    disagree_file = disagree_path,
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
    ttt_accuracy       = round(x$ttt_accuracy, 2),
    bookmaker_accuracy = round(x$bookmaker_accuracy, 2)
  )
})) %>%
  arrange(desc(ttt_accuracy))

write_csv(results_df, "accuracy_summary_ordinal_TOP100_FROMCSV_ALL_SETS.csv")

cat("Files written (per set):\n")
cat(" - Global_TTT_ordinal_predictions_TOP100_FROMCSV_<SET>.xlsx\n")
cat(" - bet365_right_ttt_wrong_ordinal_TOP100_FROMCSV_<SET>.csv\n")
cat(" - accuracy_summary_ordinal_TOP100_FROMCSV_<SET>.csv\n")
cat("Combined summary:\n")
cat(" - accuracy_summary_ordinal_TOP100_FROMCSV_ALL_SETS.csv\n")

