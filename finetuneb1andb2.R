library(readxl)
library(dplyr)
library(purrr)
library(TrueSkillThroughTimeOrdinal)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)
library(tibble)

beta   <- 1
sigma0 <- 4
gamma0 <- 0.006

EPS0   <- 0.01
ITERS0 <- 4

B1_GRID <- c(0.5, 0.6, 0.7)
B2_GRID <- c(1.4, 1.6)

df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Wsets   = suppressWarnings(as.integer(Wsets)),
    Lsets   = suppressWarnings(as.integer(Lsets)),
    Comment = trimws(Comment)
  ) %>%
  mutate(
    is_walkover = (Comment == "Walkover") | is.na(Wsets) | is.na(Lsets),
    margin_raw  = Wsets - Lsets,
    margin      = ifelse(is_walkover, NA_integer_, pmin(pmax(margin_raw, 1), 3))
  )

train_2023 <- df0 %>% filter(year(Date) == 2023)

ks <- df0 %>%
  filter(year(Date) == 2024) %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

lc_to_skill_tbl <- function(lc) {
  map_dfr(names(lc), function(dim) {
    last <- lc[[dim]][[length(lc[[dim]])]][[2]]
    tibble(player_dim = dim, mu = last@mu, sigma = last@sigma)
  }) %>%
    separate(player_dim, into = c("player","skill"), sep = "_", extra = "merge") %>%
    filter(skill == "generic") %>%
    transmute(player, mu, sigma)
}

get_mu_sigma <- function(skill_tbl, players, mu0 = 0, sigma0 = 4) {
  skill_tbl %>%
    right_join(tibble(player = players), by = "player") %>%
    mutate(
      mu    = ifelse(is.na(mu), mu0, mu),
      sigma = ifelse(is.na(sigma), sigma0, sigma)
    )
}

p_win <- function(mu_a, sg_a, mu_b, sg_b, beta) {
  denom <- sqrt(2 * beta^2 + sg_a^2 + sg_b^2)
  pnorm((mu_a - mu_b) / denom)
}

grid_results <- list()

for (B1 in B1_GRID) {
  for (B2 in B2_GRID) {
    
    if (B1 >= B2) next
    
    train_pool <- train_2023
    acc_by_k <- list()
    n_by_k   <- list()
    
    for (k in ks) {
      
      test_df <- df0 %>% filter(year(Date) == 2024, ATP == k)
      if (nrow(test_df) == 0) next
      
      # Train on played matches only (exclude walkovers from updates)
      train_model <- train_pool %>% filter(!is_walkover)
      if (nrow(train_model) == 0) {
        # nothing to train on yet; still roll pool forward
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
      
      # safety: ordinal model cannot accept NA margins
      if (any(is.na(set_margins_tr))) stop("NA found in set_margins_tr after filtering walkovers.")
      
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
      
      m$convergence(epsilon = EPS0, iterations = ITERS0)
      
      skills_tbl <- lc_to_skill_tbl(m$learning_curves())
      
      # Evaluate on played matches only
      test_played <- test_df %>% filter(!is_walkover)
      if (nrow(test_played) == 0) {
        train_pool <- bind_rows(train_pool, test_df)
        next
      }
      
      players_needed <- unique(c(test_played$Winner, test_played$Loser))
      ms <- get_mu_sigma(skills_tbl, players_needed, mu0 = 0, sigma0 = sigma0)
      
      preds <- test_played %>%
        left_join(ms %>% rename(mu_w = mu, sg_w = sigma), by = c("Winner" = "player")) %>%
        left_join(ms %>% rename(mu_l = mu, sg_l = sigma), by = c("Loser"  = "player")) %>%
        mutate(
          p_wins = p_win(mu_w, sg_w, mu_l, sg_l, beta),
          pred_winner_is_actual = (p_wins >= 0.5)
        )
      
      acc_by_k[[as.character(k)]] <- mean(preds$pred_winner_is_actual, na.rm = TRUE)
      n_by_k[[as.character(k)]]   <- nrow(preds)
      
      # Roll forward (keep walkovers in pool, but they won't train)
      train_pool <- bind_rows(train_pool, test_df)
    }
    
    acc_tbl <- tibble(
      ATP = as.integer(names(acc_by_k)),
      acc = unlist(acc_by_k),
      n   = unlist(n_by_k)
    ) %>% arrange(ATP)
    
    overall_acc <- with(acc_tbl, sum(acc * n) / sum(n))
    overall_n   <- sum(acc_tbl$n)
    
    cat(
      sprintf(
        "b1 = %.2f | b2 = %.2f | Matches = %d | Accuracy = %.4f\n",
        B1, B2, overall_n, overall_acc
      )
    )
    
    grid_results[[length(grid_results) + 1]] <- tibble(
      b1 = B1,
      b2 = B2,
      n_matches = overall_n,
      accuracy  = overall_acc
    )
  }
}

grid_results_df <- bind_rows(grid_results) %>%
  arrange(desc(accuracy), b1, b2)

print(grid_results_df, n = Inf)
cat("\nBest (by accuracy):\n")
print(grid_results_df %>% slice(1))

