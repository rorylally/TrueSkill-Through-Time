library(readxl)
library(dplyr)
library(purrr)
library(TrueSkillThroughTimeOrdinal)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)

beta   <- 1
B1     <- 0.8
B2     <- 1.4
sigma0 <- 4
gamma0 <- 0.006

df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Wsets   = as.integer(Wsets),
    Lsets   = as.integer(Lsets),
    Comment = trimws(Comment),
    WRank   = suppressWarnings(as.numeric(WRank)),
    LRank   = suppressWarnings(as.numeric(LRank))
  )  %>%
  mutate(
    is_walkover = (Comment == "Walkover") | is.na(Wsets) | is.na(Lsets),
    margin_raw  = Wsets - Lsets,
    margin      = ifelse(is_walkover, NA_integer_, pmin(pmax(margin_raw, 1), 3))
  )

train_pool <- df0 %>% filter(year(Date) == 2023)

ks <- df0 %>%
  filter(year(Date) == 2024) %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

skills_out <- list()

for (k in ks) {
  
  test_df <- df0 %>%
    filter(year(Date) == 2024, ATP == k)
  if (nrow(test_df) == 0) next
  
  train_model <- train_pool %>%
    filter(!is_walkover, !is.na(margin))
  
  if (nrow(train_model) == 0) {
    train_pool <- bind_rows(train_pool, test_df)
    next
  }
  
  comp <- map(seq_len(nrow(train_model)), function(i) {
    list(
      c(paste0(train_model$Winner[i], "_generic")),
      c(paste0(train_model$Loser[i],  "_generic"))
    )
  })
  
  times_tr       <- as.numeric(train_model$Date)
  set_margins_tr <- train_model$margin
  
  m <- TrueSkillThroughTimeOrdinal::History(
    composition = comp,
    times       = times_tr,
    sigma       = sigma0,
    gamma       = gamma0,
    beta        = beta,
    p_draw      = 0,
    set_margins = set_margins_tr,
    b1          = B1,
    b2          = B2
  )
  
  m$convergence(epsilon = 0.01, iterations = 4)
  
  
  lc <- m$learning_curves()
  
  stats_df <- map_dfr(names(lc), function(dim) {
    last <- lc[[dim]][[length(lc[[dim]])]][[2]]
    tibble(player_dim = dim, mu = last@mu, sigma = last@sigma)
  }) %>%
    separate(player_dim, into = c("player","skill"), sep = "_", extra = "merge") %>%
    filter(skill == "generic") %>%
    transmute(
      player,
      mu_generic = mu,
      sigma_generic = sigma,
      ATP = k
    )
  
  skills_out[[length(skills_out) + 1]] <- stats_df
  
  train_pool <- bind_rows(train_pool, test_df)
}

skills_snapshot_ordinal <- bind_rows(skills_out)
write_csv(skills_snapshot_ordinal, "skills_snapshot_ordinal2v2.csv")

cat("Saved: skills_snapshot_ordinal2v2.csv\n")

