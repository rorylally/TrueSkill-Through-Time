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
  filter(year(Date) == 2024) %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

skills_by_k <- list()

for (k in ks) {
  
  test_df <- df0 %>%
    filter(year(Date) == 2024, ATP == k)
  if (nrow(test_df) == 0) next
  
  # --- train TTT on current train_pool ---
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
  m$convergence(epsilon = 0.01, iterations = 4)
  
  lc <- m$learning_curves()
  
  stats_df <- map_dfr(names(lc), function(dim) {
    last <- lc[[dim]][[length(lc[[dim]])]][[2]]
    tibble(player_dim = dim, mu = last@mu, sigma = last@sigma)
  }) %>%
    separate(player_dim, into = c("player","skill"), sep = "_", extra = "merge") %>%
    filter(skill == "generic") %>%
    transmute(player, mu_generic = mu, sigma_generic = sigma)
  
  all_players <- unique(c(train_pool$Winner, train_pool$Loser,
                          test_df$Winner,  test_df$Loser))
  
  stats_wide <- tibble(player = all_players) %>%
    left_join(stats_df, by = "player") %>%
    replace_na(list(mu_generic = 0, sigma_generic = 4)) %>%
    mutate(ATP = k)
  
  skills_by_k[[length(skills_by_k) + 1]] <- stats_wide
  
  # expand training pool exactly like your pipeline
  train_pool <- bind_rows(train_pool, test_df)
}

skills_by_k_df <- bind_rows(skills_by_k)
write_csv(skills_by_k_df, "skills_snapshot_by_ATPv2.csv")
