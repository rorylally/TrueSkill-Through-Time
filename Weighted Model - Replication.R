library(readxl)
library(dplyr)
library(purrr)
library(TrueSkillThroughTime)
library(tidyr)
library(lubridate)
library(stringr)
library(writexl)
library(readr)

df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Comment = trimws(Comment),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    Court   = trimws(Court),
    
    WRank = suppressWarnings(as.numeric(WRank)),
    LRank = suppressWarnings(as.numeric(LRank))
  ) 

train_pool <- df0 %>%
  filter(year(Date) == 2023)

ks <- df0 %>%
  filter(year(Date) == 2024) %>% 
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

all_preds <- list()

for (k in ks) {
  test_df <- df0 %>%
    filter(
      year(Date) == 2024,
      ATP == k,
    )
  if (nrow(test_df) == 0) next
  
  # Add a margin based on the aggregate of Wsets and Lsets
  train_pool <- train_pool %>%
    mutate(
      set_margin = Wsets - Lsets,                              
      set_margin = if_else(is.na(set_margin), 0, set_margin),    
      set_margin = as.integer(set_margin),
      weight_raw = 1L + set_margin,                              
      weight     = pmin(4L, pmax(1L, weight_raw))                
    )
  
  #Add additional matches to the training set to increase skill
  id_expanded <- rep(seq_len(nrow(train_pool)), times = train_pool$weight)
  
  comp <- purrr::map(id_expanded, function(i) {
    w <- train_pool$Winner[i]
    l <- train_pool$Loser[i]
    list(
      c(paste0(w, "_generic")),
      c(paste0(l, "_generic"))
    )
  })
  
  times_tr <- as.numeric(train_pool$Date[id_expanded])
  
  m <- History(
    composition = comp,
    times       = times_tr,
    sigma       = 4,    # had been 8.0
    gamma       = 0.006  # had been 0.006
  )
  m$convergence(epsilon = 0.01, iterations = 4)
  
  lc <- m$learning_curves()
  stats_df <- purrr::map_dfr(names(lc), function(dim) {
    recs <- lc[[dim]]
    last <- recs[[length(recs)]][[2]]
    tibble(
      player_dim = dim,
      mu         = last@mu,
      sigma      = last@sigma
    )
  }) %>%
    tidyr::separate(player_dim, into = c("player","skill"), sep = "_", extra = "merge") %>%
    tidyr::pivot_wider(
      names_from  = skill,
      values_from = c(mu, sigma),
      names_sep   = "_"
    )
  
  all_pl <- unique(c(train_pool$Winner, train_pool$Loser,
                     test_df$Winner,  test_df$Loser))
  stats_wide <- tibble(player = all_pl) %>%
    left_join(stats_df, by = "player") %>%
    tidyr::replace_na(list(mu_generic = 0, sigma_generic = 4)) # had been 8.0
  
  preds <- test_df %>%
    left_join(stats_wide, by = c("Winner" = "player")) %>%
    left_join(stats_wide, by = c("Loser"  = "player"), suffix = c("_w","_l")) %>%
    mutate(
      mu_winner    = mu_generic_w,
      mu_loser     = mu_generic_l,
      sigma_winner = sigma_generic_w,
      sigma_loser  = sigma_generic_l,
      
      .denom_raw   = sqrt(pmax(sigma_winner^2 + sigma_loser^2, 0)),
      .denom_fix   = if_else(is.finite(.denom_raw) & .denom_raw > 0, .denom_raw, 1e-9),
      .z_val       = (mu_winner - mu_loser) / .denom_fix,
      TTT_Prob_W   = pnorm(.z_val),
      TTT_Prob_L   = 1 - TTT_Prob_W,
      TTT_Pct_W    = round(pmin(pmax(TTT_Prob_W, 0), 1) * 100, 2),
      TTT_Pct_L    = round(pmin(pmax(TTT_Prob_L, 0), 1) * 100, 2),
      
      pred         = if_else(mu_winner >= mu_loser, Winner, Loser),
      ttt_correct  = (pred == Winner),
      ATPk         = k,
      
      bookie_pred = case_when(
        !is.na(B365W) & !is.na(B365L) & B365W < B365L ~ Winner,
        !is.na(B365W) & !is.na(B365L) & B365W > B365L ~ Loser,
        TRUE ~ NA_character_
      ),
      bookie_correct = !is.na(bookie_pred) & bookie_pred == Winner,
      
      bet365_right_ttt_wrong = bookie_correct & !ttt_correct
    ) %>%
    select(
      Date, ATP, Tournament, Round, Surface, Court,
      Winner, Loser,
      any_of(c("B365W","B365L")),
      mu_winner, mu_loser, sigma_winner, sigma_loser,
      TTT_Prob_W, TTT_Prob_L, TTT_Pct_W, TTT_Pct_L,
      pred, ttt_correct, bookie_pred, bookie_correct, bet365_right_ttt_wrong,
      ATPk
    )
  
  all_preds[[length(all_preds) + 1]] <- preds
  
  train_pool <- bind_rows(train_pool, test_df)
}

all_df <- bind_rows(all_preds)

comp_df <- all_df %>%
  filter(!is.na(B365W), !is.na(B365L), B365W > 0, B365L > 0, B365W != B365L)

ttt_acc  <- mean(comp_df$ttt_correct, na.rm = TRUE) * 100
book_acc <- mean(comp_df$B365W < comp_df$B365L, na.rm = TRUE) * 100
n_comp   <- nrow(comp_df)

cat(sprintf("Accuracy — TTT: %.2f%% | Bet365: %.2f%% (same %d matches)\n",
            ttt_acc, book_acc, n_comp))

overall_acc <- mean(all_df$ttt_correct, na.rm = TRUE) * 100
cat(sprintf("2024 ATP Global model test accuracy (expanding from 2023 R1): %.1f%%\n", overall_acc))

ttt_by_year <- all_df %>%
  mutate(Year = year(Date)) %>%
  filter(Year %in% c(2023, 2024)) %>%
  group_by(Year) %>%
  summarise(
    n_matches = n(),
    accuracy  = round(mean(ttt_correct, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) %>%
  mutate(Year = as.character(Year))

ttt_combined <- tibble(
  Year = "2023–2024 Combined",
  n_matches = nrow(all_df %>% mutate(Year = year(Date)) %>% filter(Year %in% c(2023, 2024))),
  accuracy  = round(mean((all_df %>% mutate(Year = year(Date)) %>% filter(Year %in% c(2023, 2024)))$ttt_correct, na.rm = TRUE) * 100, 2)
)

ttt_table <- bind_rows(ttt_by_year, ttt_combined)
cat("\nTTT ACCURACY (MATCHING FORMAT)\n")
print(ttt_table)
readr::write_csv(ttt_table, "ttt_accuracy_by_year.csv")

# ---- Export main results (drop internals if desired) ----
all_df_export <- all_df %>%
  select(-mu_winner, -mu_loser, -sigma_winner, -sigma_loser, -TTT_Prob_W, -TTT_Prob_L)

# Rows where Bet365 predicted correctly but TTT predicted incorrectly
disagree_df <- all_df %>% filter(bet365_right_ttt_wrong)

# Save outputs
write_csv(disagree_df, "bet365_right_ttt_wrong_base_vWeight.csv")
write.csv(all_df_export, "Global_TTT_rolling_result_base_vWeight.csv", row.names = FALSE)
write_xlsx(all_df_export, "Global_TTT_rolling_result_base_vWeight.xlsx")
cat("Files written: Global_TTT_rolling_result_base_vWeight.[csv/xlsx] and bet365_right_ttt_wrong_base_vWeight.csv\n")

# --- Bet365 directional accuracy (sanity check; independent of TTT) ---
path <- "2023_2024_ATP_Results.xlsx"

df <- read_excel(path) %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date)
  ) %>%
  filter(!is.na(B365W), !is.na(B365L), B365W > 0, B365L > 0)

df_b365 <- df %>%
  filter(B365W != B365L) %>%
  mutate(bookie_correct = B365W < B365L)

acc_by_year <- df_b365 %>%
  filter(Year %in% c(2023, 2024)) %>%
  group_by(Year) %>%
  summarise(
    n_matches = n(),
    accuracy  = round(mean(bookie_correct) * 100, 2),
    .groups = "drop"
  )

acc_combined <- tibble(
  Year = "2023–2024 Combined",
  n_matches = nrow(df_b365 %>% filter(Year %in% c(2023, 2024))),
  accuracy = round(mean((df_b365 %>% filter(Year %in% c(2023, 2024)))$bookie_correct) * 100, 2)
)

acc_table <- bind_rows(
  acc_by_year %>% mutate(Year = as.character(Year)),
  acc_combined
)

print(acc_table)
cat(sprintf("Accuracy — TTT: %.2f%% | Bet365: %.2f%% (same %d matches)\n",
            ttt_acc, book_acc, n_comp))
write_csv(acc_table, "bet365_directional_accuracy_base_vWeight.csv")
