#mov_w_scale mov_w_cap ttt_acc book_acc n_matches
#<dbl>     <dbl>   <dbl>    <dbl>     <int>
#1        0.35       3      63.0     68.4      1132
#2        0.35       4.5    63.0     68.4      1132
#3        0.35       6      63.0     68.4      1132
#4        0.2        3      62.8     68.4      1132
#5        0.6        3      62.8     68.4      1132
#6        0.2        4.5    62.8     68.4      1132
#7        0.6        4.5    62.8     68.4      1132
#8        0.2        6      62.8     68.4      1132
#9        0.6        6      62.8     68.4      1132
#10        0.5        3      62.6     68.4      1132
#11        0.5        4.5    62.6     68.4      1132
#12        0.5        6      62.6     68.4      1132

# ============================================================
# ORDINAL TTT + MOV (total-games) WEIGHT GRID SEARCH
# - Same rolling tournament-style evaluation as your baseline
# - Train on expanding pool (start: 2023), test: 2024 1st Round per ATP code
# - Uses your MODIFIED TrueSkillThroughTime source (History/Batch accept:
#   use_mov_weight, mov_w_scale, mov_w_cap)
# - Uses total-games results (W1..W5 / L1..L5) to compute per-match weight
# - Prints a final grid results table + best params
# - Writes 2 CSVs: grid_results + bet365_right_ttt_wrong for best params
# ============================================================

library(readxl)
library(dplyr)
library(purrr)
library(TrueSkillThroughTimeGameAggregate2)
library(tidyr)
library(lubridate)
library(readr)

# ----------------------------
# 0) TUNING GRID
# ----------------------------
MOV_W_SCALE_GRID <- c(0.2, 0.35, 0.5, 0.6)
MOV_W_CAP_GRID   <- c(3.0, 4.5, 6.0)

# Model hyperparams (keep consistent with your baseline)
SIGMA0 <- 4
GAMMA0 <- 0.006

# Convergence controls
EPS0   <- 0.01
ITERS0 <- 4

# ----------------------------
# 1) Load & prep data
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    Court   = trimws(Court)
  )

# Ensure set-score columns exist and are numeric
w_cols <- paste0("W", 1:5)
l_cols <- paste0("L", 1:5)

missing_cols <- setdiff(c("Winner", "Loser", "Date", w_cols, l_cols), names(df0))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

df0 <- df0 %>%
  mutate(
    across(all_of(w_cols), ~ suppressWarnings(as.numeric(.x))),
    across(all_of(l_cols), ~ suppressWarnings(as.numeric(.x)))
  ) %>%
  mutate(
    games_w = rowSums(across(all_of(w_cols)), na.rm = TRUE),
    games_l = rowSums(across(all_of(l_cols)), na.rm = TRUE)
  )

# Training pool starts as 2023
train_pool0 <- df0 %>% filter(year(Date) == 2023)

# 2024 tournament list (same as your baseline)
ks <- df0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ----------------------------
# 2) Helper to fit model on current train pool and return skill table
# ----------------------------
fit_ttt_and_get_skills <- function(train_pool, mov_w_scale, mov_w_cap){
  
  # Build composition + results (TOTAL GAMES) from train_pool
  comp <- map(seq_len(nrow(train_pool)), function(i) {
    list(
      c(paste0(train_pool$Winner[i], "_generic")),
      c(paste0(train_pool$Loser[i],  "_generic"))
    )
  })
  
  results_tr <- map(seq_len(nrow(train_pool)), function(i) {
    c(train_pool$games_w[i], train_pool$games_l[i])
  })
  
  # Keep only valid scored matches
  valid <- map_lgl(results_tr, ~ {
    is.finite(.x[1]) && is.finite(.x[2]) && (.x[1] + .x[2] > 0) && (.x[1] != .x[2])
  })
  
  comp       <- comp[valid]
  results_tr <- results_tr[valid]
  
  # Times
  times_tr <- as.numeric(train_pool$Date)[valid]
  
  # Fit
  m <- History(
    composition    = comp,
    results        = results_tr,
    times          = times_tr,
    sigma          = SIGMA0,
    gamma          = GAMMA0,
    use_mov_weight = TRUE,
    mov_w_scale    = mov_w_scale,
    mov_w_cap      = mov_w_cap
  )
  m$convergence(epsilon = EPS0, iterations = ITERS0)
  
  # Extract last posterior per dimension -> player-level stats
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
  
  stats_df
}

# ----------------------------
# 3) Evaluate one grid point (mov_w_scale, mov_w_cap)
# ----------------------------
run_one_grid <- function(mov_w_scale, mov_w_cap){
  
  train_pool <- train_pool0
  all_preds  <- list()
  
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(year(Date) == 2024, ATP == k, Round == "1st Round")
    if (nrow(test_df) == 0) next
    
    stats_df <- fit_ttt_and_get_skills(train_pool, mov_w_scale, mov_w_cap)
    
    all_players <- unique(c(train_pool$Winner, train_pool$Loser,
                            test_df$Winner,  test_df$Loser))
    
    stats_wide <- tibble(player = all_players) %>%
      left_join(stats_df, by = "player") %>%
      replace_na(list(mu_generic = 0, sigma_generic = SIGMA0))
    
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
    
    # Expanding pool
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
  
  list(
    params = tibble(
      mov_w_scale = mov_w_scale,
      mov_w_cap   = mov_w_cap,
      ttt_acc     = round(ttt_acc, 4),
      book_acc    = round(book_acc, 4),
      n_matches   = n_comp
    ),
    preds = comp_df
  )
}

# ----------------------------
# 4) Run grid search
# ----------------------------
grid <- expand.grid(
  mov_w_scale = MOV_W_SCALE_GRID,
  mov_w_cap   = MOV_W_CAP_GRID
) %>% as_tibble()

grid_results <- list()
best_preds   <- NULL
best_row     <- NULL

for (i in seq_len(nrow(grid))) {
  
  s   <- grid$mov_w_scale[i]
  cap <- grid$mov_w_cap[i]
  
  cat(sprintf("Running grid %d/%d | mov_w_scale=%.3f mov_w_cap=%.2f\n",
              i, nrow(grid), s, cap))
  
  out <- run_one_grid(s, cap)
  
  grid_results[[i]] <- out$params
  
  if (is.null(best_row) || out$params$ttt_acc > best_row$ttt_acc) {
    best_row   <- out$params
    best_preds <- out$preds
  }
  
  cat(sprintf("  -> Accuracy — TTT: %.2f%% | Bookmakers: %.2f%% (same %d matches)\n\n",
              out$params$ttt_acc, out$params$book_acc, out$params$n_matches))
}

res_df <- bind_rows(grid_results) %>%
  arrange(desc(ttt_acc), desc(n_matches))

print(res_df)

cat("\nBEST PARAMS:\n")
print(best_row)

# ----------------------------
# 5) Save outputs (best only + grid table)
# ----------------------------
write_csv(res_df, "grid_results_MOV_WEIGHT_total_games.csv")


cat("Saved:\n")
cat(" - grid_results_MOV_WEIGHT_total_games.csv\n")



