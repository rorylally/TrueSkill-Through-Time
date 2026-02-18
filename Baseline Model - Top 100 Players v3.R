#[All_2024_R1] Accuracy — TTT (from CSV skills): 62.73% | Bookmakers: 65.60% (same 593 matches)
#[ATP250] Accuracy — TTT (from CSV skills): 56.42% | Bookmakers: 62.01% (same 179 matches)                    
#[ATP500] Accuracy — TTT (from CSV skills): 63.11% | Bookmakers: 62.30% (same 122 matches)                    
#[Masters1000] Accuracy — TTT (from CSV skills): 59.29% | Bookmakers: 64.29% (same 140 matches)               
#[Grandslams] Accuracy — TTT (from CSV skills): 75.00% | Bookmakers: 76.52% (same 132 matches) 

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

# ----------------------------
# 0) Tournament sets (your input)
# ----------------------------
TOURNAMENT_SETS <- list(
  All_2024_R1 = NULL,
  ATP250 = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27, 30, 31, 33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53, 57, 58, 59, 63, 64),
  ATP500 = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60 ,61),
  Masters1000 = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams = c(5, 32, 39, 51)
)

# ----------------------------
# 1) Load data
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = str_trim(Winner),
    Loser   = str_trim(Loser),
    Surface = str_trim(Surface),
    Court   = str_trim(Court),
    
    WRank = suppressWarnings(as.numeric(WRank)),
    LRank = suppressWarnings(as.numeric(LRank))
  )


skills <- read_csv("skills_snapshot_by_ATP.csv", show_col_types = FALSE) %>%
  mutate(player = str_trim(player))

# ----------------------------
# 2) Base tournament list available in 2024 R1
# ----------------------------
ks_all <- df0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ----------------------------
# 3) Runner for one set of tournaments
# ----------------------------
run_baseline_for_set <- function(set_name, atp_vec_or_null) {
  
  # Decide which ATPs to run
  ks <- if (is.null(atp_vec_or_null)) {
    ks_all
  } else {
    sort(intersect(ks_all, atp_vec_or_null))
  }
  
  all_preds <- list()
  
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(year(Date) == 2024, ATP == k, Round == "1st Round", !is.na(WRank), !is.na(LRank),
             WRank <= 100,
             LRank <= 100)
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
        
        z_val = (mu_winner - mu_loser) /
          sqrt(pmax(sigma_winner^2 + sigma_loser^2, 1e-9)),
        
        pred        = if_else(mu_winner >= mu_loser, Winner, Loser),
        ttt_correct = pred == Winner,
        
        bookie_pred = case_when(
          !is.na(B365W) & !is.na(B365L) & B365W < B365L ~ Winner,
          !is.na(B365W) & !is.na(B365L) & B365W > B365L ~ Loser,
          TRUE ~ NA_character_
        ),
        bookie_correct = !is.na(bookie_pred) & bookie_pred == Winner
      )
    
    all_preds[[length(all_preds) + 1]] <- preds
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
  
  ttt_acc  <- mean(comp_df$ttt_correct) * 100
  book_acc <- mean(comp_df$bookie_correct) * 100
  n_comp   <- nrow(comp_df)
  
  cat(sprintf(
    "[%s] Accuracy — TTT (from CSV skills): %.2f%% | Bookmakers: %.2f%% (same %d matches)\n",
    set_name, ttt_acc, book_acc, n_comp
  ))
  
  # Save outputs (tagged by set name)
  disagree_path <- sprintf("bet365_right_ttt_wrong_BASELINEv5_%s.csv", set_name)
  summary_path  <- sprintf("accuracy_summary_BASELINEv5_%s.csv", set_name)
  
  disagree_df <- comp_df %>% filter(bookie_correct & !ttt_correct)
  write_csv(disagree_df, disagree_path)
  
  accuracy_summary <- tibble(
    tournament_set      = set_name,
    ttt_accuracy        = round(ttt_acc, 2),
    bookmaker_accuracy  = round(book_acc, 2),
    n_matches           = n_comp,
    n_tournaments_used  = length(ks)
  )
  write_csv(accuracy_summary, summary_path)
  
  list(
    set_name = set_name,
    ks = ks,
    ttt_acc = ttt_acc,
    book_acc = book_acc,
    n_comp = n_comp,
    disagree_path = disagree_path,
    summary_path = summary_path
  )
}

# ----------------------------
# 4) Run for every tournament set + write a combined summary
# ----------------------------
results <- lapply(names(TOURNAMENT_SETS), function(nm) {
  run_baseline_for_set(nm, TOURNAMENT_SETS[[nm]])
})

results_df <- bind_rows(lapply(results, function(x) {
  tibble(
    tournament_set     = x$set_name,
    n_tournaments_used = length(x$ks),
    n_matches          = x$n_comp,
    ttt_accuracy       = round(x$ttt_acc, 2),
    bookmaker_accuracy = round(x$book_acc, 2)
  )
})) %>%
  arrange(desc(ttt_accuracy))

write_csv(results_df, "accuracy_summary_BASELINE_TOP100v3.csv")