#Accuracy — TTT (Top-100; from CSV skills): 62.73% | Bookmakers: 65.60% (same 593 matches)

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)


df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    Court   = trimws(Court),
    WRank   = suppressWarnings(as.numeric(WRank)),
    LRank   = suppressWarnings(as.numeric(LRank))
  ) %>%
  filter(
    !is.na(WRank), !is.na(LRank),
    WRank <= 100, LRank <= 100
  )


skills <- read_csv("skills_snapshot_by_ATP.csv", show_col_types = FALSE) %>%
  mutate(player = trimws(player))

ks <- df0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

all_preds <- list()


for (k in ks) {
  
  test_df <- df0 %>%
    filter(year(Date) == 2024, ATP == k, Round == "1st Round")
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
  "Accuracy — TTT (Top-100; from CSV skills): %.2f%% | Bookmakers: %.2f%% (same %d matches)\n",
  ttt_acc, book_acc, n_comp
))


disagree_df <- comp_df %>% filter(bookie_correct & !ttt_correct)
write_csv(disagree_df, "bet365_right_ttt_wrong_BASELINE_TOP100v2.csv")

accuracy_summary <- tibble(
  ttt_accuracy        = round(ttt_acc, 2),
  bookmaker_accuracy  = round(book_acc, 2),
  n_matches           = n_comp
)
write_csv(accuracy_summary, "accuracy_summary_BASELINE_TOP100v2.csv")

cat("Saved:\n")
cat(" - bet365_right_ttt_wrong_BASELINE_TOP100v2.csv\n")
cat(" - accuracy_summary_BASELINE_TOP100v2.csv\n")
