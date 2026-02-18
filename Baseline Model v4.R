library(readxl)
library(dplyr)
library(purrr)
library(TrueSkillThroughTime)
library(tidyr)
library(lubridate)
library(readr)

df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    Court   = trimws(Court)
  )

train_pool <- df0 %>% filter(year(Date) == 2023)

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
  
  comp <- map(seq_len(nrow(train_pool)), function(i) {
    list(
      c(paste0(train_pool$Winner[i], "_generic")),
      c(paste0(train_pool$Loser[i],  "_generic"))
    )
  })
  times_tr <- as.numeric(train_pool$Date)
  
  m <- History(
    composition = comp,
    times       = times_tr,
    sigma       = 4,
    gamma       = 0.006
  )
  m$convergence(epsilon = 0.01, iterations = 6)
  
  lc <- m$learning_curves()
  stats_df <- map_dfr(names(lc), function(dim) {
    last <- lc[[dim]][[length(lc[[dim]])]][[2]]
    tibble(player_dim = dim, mu = last@mu, sigma = last@sigma)
  }) %>%
    separate(player_dim, into = c("player","skill"), sep = "_", extra = "merge") %>%
    pivot_wider(
      names_from = skill,
      values_from = c(mu, sigma),
      names_sep = "_"
    )
  
  all_players <- unique(c(train_pool$Winner, train_pool$Loser,
                          test_df$Winner,  test_df$Loser))
  
  stats_wide <- tibble(player = all_players) %>%
    left_join(stats_df, by = "player") %>%
    replace_na(list(mu_generic = 0, sigma_generic = 4))
  
  preds <- test_df %>%
    left_join(stats_wide, by = c("Winner" = "player")) %>%
    left_join(stats_wide, by = c("Loser"  = "player"), suffix = c("_w","_l")) %>%
    mutate(
      mu_winner    = mu_generic_w,
      mu_loser     = mu_generic_l,
      sigma_winner = sigma_generic_w,
      sigma_loser  = sigma_generic_l,
      
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

ttt_acc  <- mean(comp_df$ttt_correct) * 100
book_acc <- mean(comp_df$bookie_correct) * 100
n_comp   <- nrow(comp_df)

cat(sprintf(
  "Accuracy — TTT: %.2f%% | Bookmakers: %.2f%% (same %d matches)\n",
  ttt_acc, book_acc, n_comp
))

disagree_df <- comp_df %>% filter(bookie_correct & !ttt_correct)
write_csv(disagree_df, "bet365_right_ttt_wrong_BASELINEv4.csv")

accuracy_summary <- tibble(
  ttt_accuracy        = round(ttt_acc, 2),
  bookmaker_accuracy = round(book_acc, 2),
  n_matches           = n_comp
)
write_csv(accuracy_summary, "accuracy_summary_BASELINEv4.csv")

cat("Saved:\n")
cat(" - bet365_right_ttt_wrong_BASELINEv4.csv\n")
cat(" - accuracy_summary_BASELINEv4.csv\n")
