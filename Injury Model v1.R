# ============================
# TTT rolling evaluation + Injury μ-discount (step)
# GRID SEARCH over:
#   - DAYS_WINDOW (days since last Retired/Walkover)
#   - LAMBDA (μ discount)
# Prints ALL results at the end (and saves grid table)
# ============================

library(readxl)
library(dplyr)
library(purrr)
library(TrueSkillThroughTime)
library(tidyr)
library(lubridate)
library(readr)

# ----------------------------
# 0) GRID (edit these)
# ----------------------------
DAYS_WINDOW_GRID <- c(7, 14, 30, 45, 60, 90)
LAMBDA_GRID      <- c(0.25, 0.50, 0.75)

# ----------------------------
# 1) Load & prep data (once)
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Comment = if_else(is.na(Comment), "", as.character(Comment)),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    Court   = trimws(Court)
  )

ks <- df0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ----------------------------
# 2) Helper: run one combo
# ----------------------------
run_one_combo <- function(DAYS_WINDOW, LAMBDA) {
  
  train_pool <- df0 %>% filter(year(Date) == 2023)
  all_preds  <- list()
  
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(year(Date) == 2024, ATP == k, Round == "1st Round")
    if (nrow(test_df) == 0) next
    
    # ---- Injury proxy (clean rule): Comment contains Retired/Walkover; injured player = Loser
    tp_ret <- train_pool %>%
      filter(grepl("Retired|Walkover", Comment, ignore.case = TRUE))
    
    ret_last <- tp_ret %>%
      group_by(player = Loser) %>%
      summarise(last_retired_date = max(Date), .groups = "drop")
    
    anchor_date <- min(test_df$Date, na.rm = TRUE)
    
    inj_tbl <- tibble(player = unique(c(test_df$Winner, test_df$Loser))) %>%
      left_join(ret_last, by = "player") %>%
      mutate(
        days_since_ret = as.numeric(anchor_date - last_retired_date),
        inj_flag = case_when(
          is.na(days_since_ret) ~ 0L,
          days_since_ret >= 0 & days_since_ret <= DAYS_WINDOW ~ 1L,
          TRUE ~ 0L
        )
      ) %>%
      select(player, inj_flag)
    
    # ---- Build TTT training history
    comp <- map(seq_len(nrow(train_pool)), function(i) {
      list(
        c(paste0(train_pool$Winner[i], "_generic")),
        c(paste0(train_pool$Loser[i],  "_generic"))
      )
    })
    times_tr <- as.numeric(train_pool$Date)
    
    # ---- Fit TTT
    m <- History(
      composition = comp,
      times       = times_tr,
      sigma       = 4,
      gamma       = 0.006
    )
    m$convergence(epsilon = 0.01, iterations = 4)
    
    # ---- Extract latest μ/σ
    lc <- m$learning_curves()
    stats_df <- map_dfr(names(lc), function(dim) {
      last <- lc[[dim]][[length(lc[[dim]])]][[2]]
      tibble(player_dim = dim, mu = last@mu, sigma = last@sigma)
    }) %>%
      separate(player_dim, into = c("player","skill"), sep = "_", extra = "merge") %>%
      pivot_wider(
        names_from  = skill,
        values_from = c(mu, sigma),
        names_sep   = "_"
      )
    
    all_players <- unique(c(train_pool$Winner, train_pool$Loser,
                            test_df$Winner,  test_df$Loser))
    
    stats_wide <- tibble(player = all_players) %>%
      left_join(stats_df, by = "player") %>%
      replace_na(list(mu_generic = 0, sigma_generic = 4))
    
    # ---- Predict (μ discount only)
    preds <- test_df %>%
      left_join(stats_wide, by = c("Winner" = "player")) %>%
      left_join(stats_wide, by = c("Loser"  = "player"), suffix = c("_w","_l")) %>%
      left_join(inj_tbl,    by = c("Winner" = "player")) %>%
      left_join(inj_tbl,    by = c("Loser"  = "player"), suffix = c("_w_inj","_l_inj")) %>%
      mutate(
        mu_winner    = mu_generic_w,
        mu_loser     = mu_generic_l,
        sigma_winner = sigma_generic_w,
        sigma_loser  = sigma_generic_l,
        
        inj_w = coalesce(inj_flag_w_inj, 0L),
        inj_l = coalesce(inj_flag_l_inj, 0L),
        
        mu_winner_adj = mu_winner - LAMBDA * inj_w,
        mu_loser_adj  = mu_loser  - LAMBDA * inj_l,
        
        pred        = if_else(mu_winner_adj >= mu_loser_adj, Winner, Loser),
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
  
  tibble(
    days_window = DAYS_WINDOW,
    lambda      = LAMBDA,
    n_matches   = nrow(comp_df),
    ttt_acc     = round(mean(comp_df$ttt_correct)  * 100, 4),
    book_acc    = round(mean(comp_df$bookie_correct) * 100, 4),
    # how often injury actually triggered on the evaluated set (sanity signal)
    inj_rate    = round(mean((coalesce(comp_df$inj_w, 0L) == 1L) | (coalesce(comp_df$inj_l, 0L) == 1L)) * 100, 4)
  )
}

# ----------------------------
# 3) Run grid + collect results
# ----------------------------
grid <- expand.grid(
  DAYS_WINDOW = DAYS_WINDOW_GRID,
  LAMBDA      = LAMBDA_GRID
)

res_list <- vector("list", nrow(grid))

for (i in seq_len(nrow(grid))) {
  dw <- grid$DAYS_WINDOW[i]
  lb <- grid$LAMBDA[i]
  cat(sprintf("Running combo %d/%d: DAYS_WINDOW=%d | LAMBDA=%.2f\n", i, nrow(grid), dw, lb))
  res_list[[i]] <- run_one_combo(DAYS_WINDOW = dw, LAMBDA = lb)
}

res_df <- bind_rows(res_list) %>%
  arrange(desc(ttt_acc), desc(n_matches))

# ----------------------------
# 4) Print + save
# ----------------------------
cat("\n=== GRID SEARCH RESULTS (sorted by TTT accuracy) ===\n")
print(res_df, n = nrow(res_df))

write_csv(res_df, "injury_grid_results.csv")
cat("\nSaved: injury_grid_results.csv\n")


