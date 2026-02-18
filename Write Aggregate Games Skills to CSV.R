library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(readr)

# ============================================================
# SKILLS SNAPSHOT BY ATP TOURNAMENT (1st Round 2024)
# Uses your rolling pipeline style (train_pool expands)
# Saves final (mu, sigma) snapshot per tournament k to CSV
# Assumes your weighted-EP TTT source (the modified History/Game)
# is already sourced/loaded in this R session.
# ============================================================

# --- Load & prep data ---
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    Court   = trimws(Court)
  )

# ---- Seed training with 2023 ----
train_pool <- df0 %>% filter(year(Date) == 2023)

# ---- 2024 tournament codes (1st Round only) ----
ks <- df0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ============================================================
# Helper: build composition/results/times from tennis W1..L5
# Results is c(total_games_winner, total_games_loser)
# ============================================================
build_ttt_inputs_tennis <- function(
    df,
    winner_col = "Winner",
    loser_col  = "Loser",
    date_col   = "Date",
    w_prefix   = "W",
    l_prefix   = "L",
    max_sets   = 5L,
    time_mode  = c("date_numeric", "sequence"),
    time_origin = "1970-01-01"
) {
  time_mode <- match.arg(time_mode)
  
  w_cols <- paste0(w_prefix, seq_len(max_sets))
  l_cols <- paste0(l_prefix, seq_len(max_sets))
  
  missing_cols <- setdiff(c(winner_col, loser_col, date_col, w_cols, l_cols), names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  for (cc in c(w_cols, l_cols)) {
    df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
  }
  
  games_w <- rowSums(df[w_cols], na.rm = TRUE)
  games_l <- rowSums(df[l_cols], na.rm = TRUE)
  
  valid_score <- (games_w + games_l) > 0 & (games_w != games_l)
  
  
  composition <- vector("list", nrow(df))
  results     <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    
    if (!valid_score[i]) {
      
      composition[[i]] <- NULL
      results[[i]]     <- NULL
      next
    }
    
    wname <- as.character(df[[winner_col]][i])
    lname <- as.character(df[[loser_col]][i])
    
    # match your original style: suffix "_generic"
    composition[[i]] <- list(
      c(paste0(wname, "_generic")),
      c(paste0(lname, "_generic"))
    )
    
    # IMPORTANT: for weighted engine, results should be total games
    results[[i]] <- c(games_w[i], games_l[i])
  }
  
  if (time_mode == "date_numeric") {
    d <- as.Date(df[[date_col]])
    times <- as.numeric(d - as.Date(time_origin))
  } else {
    times <- seq_len(nrow(df))
  }
  
  keep <- which(valid_score)
  list(
    composition = composition[keep],
    results     = results[keep],
    times       = times[keep],
    games_w     = games_w[keep],
    games_l     = games_l[keep]
  )
}

# ============================================================
# Rolling loop: fit on expanding train_pool, snapshot skills
# ============================================================
skills_by_k <- list()

for (k in ks) {
  
  test_df <- df0 %>%
    filter(year(Date) == 2024, ATP == k, Round == "1st Round")
  if (nrow(test_df) == 0) next
  
  # --- train on current train_pool ---
  inp_tr <- build_ttt_inputs_tennis(train_pool, time_mode = "date_numeric")
  
  m <- History(
    composition = inp_tr$composition,
    results     = inp_tr$results,   # <-- total games (W1..W5 sums, L1..L5 sums)
    times       = inp_tr$times,
    sigma       = 4,
    gamma       = 0.006
  )
  
  m$convergence(epsilon = 0.01, iterations = 6)
  
  # --- extract last skill snapshot (same learning_curves approach) ---
  lc <- m$learning_curves()
  
  stats_df <- map_dfr(names(lc), function(dim) {
    last <- lc[[dim]][[length(lc[[dim]])]][[2]]
    tibble(player_dim = dim, mu = last@mu, sigma = last@sigma)
  }) %>%
    separate(player_dim, into = c("player","skill"), sep = "_", extra = "merge") %>%
    filter(skill == "generic") %>%
    transmute(player, mu_generic = mu, sigma_generic = sigma)
  
  # players we want in this snapshot
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

write_csv(skills_by_k_df, "skills_snapshot_by_ATP_games3.csv")
