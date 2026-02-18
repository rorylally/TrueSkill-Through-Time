# ============================

#B1  B2  ttt_acc book_acc n_comp
#1 1.0 1.2 64.24870 65.80311    579
#2 1.1 1.2 64.24870 65.80311    579
#3 1.0 1.4 63.90328 65.80311    579
#4 1.1 1.4 63.90328 65.80311    579
#5 1.2 1.4 63.73057 65.80311    579
#6 1.0 1.6 63.38515 65.80311    579
#7 1.1 1.6 63.38515 65.80311    579
#8 1.2 1.6 63.21244 65.80311    579

#B1  B2  ttt_acc book_acc n_comp
#0 0.9 1.0 64.08 65.80 579

#B1  B2  ttt_acc book_acc n_comp
#1  0.8 1.0 63.73057 65.80311    579
#2  0.8 1.6 63.03972 65.80311    579
#3  0.8 1.2 62.86701 65.80311    579
#4  0.8 1.4 62.86701 65.80311    579
#5  0.6 1.2 62.69430 65.80311    579
#6  0.2 1.0 62.52159 65.80311    579
#7  0.6 1.0 62.52159 65.80311    579
#8  0.2 1.2 62.52159 65.80311    579
#9  0.4 1.2 62.52159 65.80311    579
#10 0.4 1.4 62.52159 65.80311    579
#11 0.4 1.0 62.34888 65.80311    579
#12 0.2 1.4 62.34888 65.80311    579
#13 0.6 1.4 62.34888 65.80311    579
#14 0.8 2.0 62.34888 65.80311    579
#15 0.2 1.6 62.17617 65.80311    579
#16 0.6 1.6 62.17617 65.80311    579
#17 0.2 2.0 62.17617 65.80311    579
#18 0.6 2.0 62.17617 65.80311    579
#19 0.4 1.6 62.00345 65.80311    579
#20 0.4 2.0 61.83074 65.80311    579
# ============================
# Ordinal TTT rolling evaluation (grid-search B1,B2)
# Objective: ACCURACY ONLY (top-100, 2024 1st round tournaments)
# ============================

# --- Libraries ---
library(readxl)
library(dplyr)
library(purrr)
library(TrueSkillThroughTimeOrdinal)
library(tidyr)
library(lubridate)
library(stringr)
library(writexl)
library(readr)

# ============================
# 0) TUNING GRID (edit these)
# ============================
beta <- 1

B1_GRID <- c(1.0, 1.1, 1.2)
B2_GRID <- c(1.0, 1.2, 1.4, 1.6)

# guard: require b2 > b1
GRID <- expand.grid(B1 = B1_GRID, B2 = B2_GRID) %>%
  dplyr::filter(B2 > B1)

# ============================
# 1) Load & prep data
# ============================
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
  ) %>%
  filter(Comment == "Completed") %>%
  mutate(
    margin = Wsets - Lsets,
    margin = pmin(pmax(margin, 1), 3)
  )

# ---- Seed training with 2023 (top-100 only) ----
train_seed <- df0 %>%
  filter(
    year(Date) == 2023,
    !is.na(WRank), !is.na(LRank),
    WRank <= 100, LRank <= 100
  )

# ---- 2024 tournament codes present ----
ks <- df0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ============================
# 2) Helper: run one (B1,B2)
# ============================
run_one <- function(B1, B2){
  
  train_pool <- train_seed
  all_preds <- list()
  
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(
        year(Date) == 2024,
        ATP == k,
        Round == "1st Round",
        !is.na(WRank), !is.na(LRank),
        WRank <= 100, LRank <= 100
      )
    if (nrow(test_df) == 0) next
    
    # --- Build training composition + aligned set_margins ---
    comp <- purrr::map(seq_len(nrow(train_pool)), function(i) {
      w <- train_pool$Winner[i]
      l <- train_pool$Loser[i]
      list(c(paste0(w, "_generic")),
           c(paste0(l, "_generic")))
    })
    times_tr <- as.numeric(train_pool$Date)
    set_margins_tr <- train_pool$margin
    
    # --- Fit Ordinal TTT ---
    m <- History(
      composition = comp,
      times       = times_tr,
      sigma       = 4,
      gamma       = 0.006,
      beta        = beta,
      p_draw      = 0,
      set_margins = set_margins_tr,
      b1          = B1,
      b2          = B2
    )
    m$convergence(epsilon = 0.01, iterations = 6)
    
    # --- Extract latest μ & σ ---
    lc <- m$learning_curves()
    stats_df <- purrr::map_dfr(names(lc), function(dim) {
      recs <- lc[[dim]]
      last <- recs[[length(recs)]][[2]]
      tibble(player_dim = dim, mu = last@mu, sigma = last@sigma)
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
      tidyr::replace_na(list(mu_generic = 0, sigma_generic = 4))
    
    preds <- test_df %>%
      left_join(stats_wide, by = c("Winner" = "player")) %>%
      left_join(stats_wide, by = c("Loser"  = "player"), suffix = c("_w","_l")) %>%
      mutate(
        mu_winner    = mu_generic_w,
        mu_loser     = mu_generic_l,
        sigma_winner = sigma_generic_w,
        sigma_loser  = sigma_generic_l,
        
        .denom_raw = sqrt(pmax(2*beta^2 + sigma_winner^2 + sigma_loser^2, 0)),
        .denom_fix = if_else(is.finite(.denom_raw) & .denom_raw > 0, .denom_raw, 1e-9),
        .z_val     = (mu_winner - mu_loser) / .denom_fix,
        TTT_Prob_W = pnorm(.z_val),
        
        pred        = if_else(mu_winner >= mu_loser, Winner, Loser),
        ttt_correct = (pred == Winner),
        
        bookie_pred = case_when(
          !is.na(B365W) & !is.na(B365L) & B365W < B365L ~ Winner,
          !is.na(B365W) & !is.na(B365L) & B365W > B365L ~ Loser,
          TRUE ~ NA_character_
        ),
        bookie_correct = !is.na(bookie_pred) & bookie_pred == Winner
      )
    
    all_preds[[length(all_preds) + 1]] <- preds
    
    # expand training pool
    train_pool <- bind_rows(train_pool, test_df)
  }
  
  all_df <- bind_rows(all_preds)
  
  # apples-to-apples on matches with usable odds
  comp_df <- all_df %>%
    filter(!is.na(B365W), !is.na(B365L), B365W > 0, B365L > 0, B365W != B365L)
  
  ttt_acc  <- mean(comp_df$ttt_correct, na.rm = TRUE) * 100
  book_acc <- mean(comp_df$B365W < comp_df$B365L, na.rm = TRUE) * 100
  n_comp   <- nrow(comp_df)
  
  list(
    B1 = B1, B2 = B2,
    ttt_acc = ttt_acc,
    book_acc = book_acc,
    n_comp = n_comp
  )
}

# ============================
# 3) Grid search + progress print
# ============================
results <- vector("list", nrow(GRID))

for (i in seq_len(nrow(GRID))) {
  B1 <- GRID$B1[i]
  B2 <- GRID$B2[i]
  cat(sprintf("Testing B1=%.2f, B2=%.2f ...\n", B1, B2))
  
  results[[i]] <- run_one(B1, B2)
  
  cat(sprintf("  Done: TTT=%.2f%% | Bet365=%.2f%% (n=%d)\n",
              results[[i]]$ttt_acc, results[[i]]$book_acc, results[[i]]$n_comp))
}

res_df <- bind_rows(lapply(results, as.data.frame))

# ============================
# 4) Print best + save
# ============================
best <- res_df %>% arrange(desc(ttt_acc)) %>% slice(1)

cat("\n============================\n")
cat("BEST (by TTT accuracy)\n")
print(best)
cat("============================\n\n")

print(res_df %>% arrange(desc(ttt_acc)))

write_csv(res_df, "ordinal_b1_b2_grid_results.csv")
cat("Saved grid results to ordinal_b1_b2_grid_results.csv\n")
