# ============================
# Global TTT rolling evaluation
# + margin-based weighting scaled by a_scale
# + GRAND SLAMS ONLY via ATP-index INTERSECT
# + Fine-tune a_scale grid (prints progress + final summary + final export)
# Objective: ACCURACY ONLY
# ============================

# --- Libraries ---
library(readxl)
library(dplyr)
library(purrr)
library(TrueSkillThroughTime)
library(tidyr)
library(lubridate)
library(stringr)
library(writexl)
library(readr)

# ============================
# 0) TUNING GRID + SETTINGS
# ============================
A_SCALE_GRID <- seq(0.8, 1.5, by = 0.1)  # <-- edit this however you want
W_CAP <- 4L

# ---- Put your Grand Slam ATP indices here ----
GS_ATP <- c(5, 32, 39, 51)  # <-- replace if yours differ

# ============================
# 1) Load & prep data
# ============================
path <- "2023_2024_ATP_Results.xlsx"

df0 <- read_excel(path) %>%
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

# ---- Get 2024 tournament codes (R1) and INTERSECT to GS only ----
ks <- df0 %>%
  filter(year(Date) == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP) %>%
  intersect(GS_ATP)

cat("Grand Slam ATP indices used:", paste(GS_ATP, collapse = ", "), "\n")
cat("2024 ks after intersect:", paste(ks, collapse = ", "), "\n")

# ============================
# 2) Runner function
# ============================
run_one_scale <- function(a_scale, return_outputs = FALSE) {
  
  train_pool <- df0 %>% filter(year(Date) == 2023)
  all_preds <- list()
  
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(
        year(Date) == 2024,
        ATP == k,
        ATP %in% GS_ATP,
        Round == "1st Round",
        !is.na(WRank), !is.na(LRank),
        WRank <= 100,
        LRank <= 100
      )
    
    if (nrow(test_df) == 0) next
    
    # --- Margin-based weighting (scaled) ---
    train_pool <- train_pool %>%
      mutate(
        set_margin = Wsets - Lsets,
        set_margin = if_else(is.na(set_margin), 0, set_margin),
        set_margin = as.integer(set_margin),
        
        weight_raw = 1L + set_margin,
        weight0    = pmin(W_CAP, pmax(1L, weight_raw)),
        
        weight     = as.integer(round(weight0 * a_scale)),
        weight     = pmin(W_CAP, pmax(1L, weight))
      )
    
    idx_expanded <- rep(seq_len(nrow(train_pool)), times = train_pool$weight)
    
    comp <- purrr::map(idx_expanded, function(i) {
      w <- train_pool$Winner[i]
      l <- train_pool$Loser[i]
      list(c(paste0(w, "_generic")),
           c(paste0(l, "_generic")))
    })
    
    times_tr <- as.numeric(train_pool$Date[idx_expanded])
    
    m <- History(
      composition = comp,
      times       = times_tr,
      sigma       = 4,
      gamma       = 0.006
    )
    m$convergence(epsilon = 0.01, iterations = 4)
    
    # --- Extract μ & σ ---
    lc <- m$learning_curves()
    stats_df <- purrr::map_dfr(names(lc), function(dim) {
      recs <- lc[[dim]]
      last <- recs[[length(recs)]][[2]]
      tibble(player_dim = dim, mu = last@mu, sigma = last@sigma)
    }) %>%
      tidyr::separate(player_dim, into = c("player","skill"), sep = "_", extra = "merge") %>%
      tidyr::pivot_wider(names_from = skill, values_from = c(mu, sigma), names_sep = "_")
    
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
        bet365_right_ttt_wrong = bookie_correct & !ttt_correct,
        a_scale = a_scale
      )
    
    all_preds[[length(all_preds) + 1]] <- preds
    train_pool <- bind_rows(train_pool, test_df)
  }
  
  all_df <- bind_rows(all_preds)
  
  overall_acc <- mean(all_df$ttt_correct, na.rm = TRUE) * 100
  
  comp_df <- all_df %>%
    filter(!is.na(B365W), !is.na(B365L), B365W > 0, B365L > 0, B365W != B365L)
  
  ttt_acc  <- mean(comp_df$ttt_correct, na.rm = TRUE) * 100
  book_acc <- mean(comp_df$B365W < comp_df$B365L, na.rm = TRUE) * 100
  n_comp   <- nrow(comp_df)
  
  out <- list(
    overall_acc = overall_acc,
    ttt_acc_same_odds = ttt_acc,
    book_acc_same_odds = book_acc,
    n_comp = n_comp
  )
  if (return_outputs) out$all_df <- all_df
  out
}

# ============================
# 3) TUNE a_scale grid
# ============================
tuning_results <- purrr::map_dfr(A_SCALE_GRID, function(a_scale) {
  
  cat(sprintf(
    "\n==============================\nStarting test for a_scale = %.2f\n==============================\n",
    a_scale
  ))
  
  res <- run_one_scale(a_scale = a_scale, return_outputs = FALSE)
  
  cat(sprintf(
    "Finished a_scale = %.2f | Overall accuracy = %.2f%% | Same-odds accuracy = %.2f%% (n=%d)\n",
    a_scale, res$overall_acc, res$ttt_acc_same_odds, res$n_comp
  ))
  
  tibble(
    a_scale = a_scale,
    overall_acc = res$overall_acc,
    ttt_acc_same_odds = res$ttt_acc_same_odds,
    book_acc_same_odds = res$book_acc_same_odds,
    n_comp = res$n_comp
  )
})

write_csv(tuning_results, "ttt_weight_scaling_tuning_accuracy_GS.csv")
cat("\nWrote: ttt_weight_scaling_tuning_accuracy_GS.csv\n")

cat("\n==============================\nFINAL TUNING SUMMARY (sorted by overall_acc)\n==============================\n")
print(tuning_results %>% arrange(desc(overall_acc), desc(ttt_acc_same_odds)))

best <- tuning_results %>%
  arrange(desc(overall_acc), desc(ttt_acc_same_odds)) %>%
  slice(1)

best_a <- best$a_scale[[1]]

cat(sprintf(
  "\nBEST SELECTION: a_scale = %.2f | overall_acc = %.2f%% | same-odds acc = %.2f%% (n=%d)\n",
  best_a, best$overall_acc[[1]], best$ttt_acc_same_odds[[1]], best$n_comp[[1]]
))

# ============================
# 4) FINAL RUN with BEST a_scale + EXPORT
# ============================
final <- run_one_scale(a_scale = best_a, return_outputs = TRUE)
all_df <- final$all_df

cat(sprintf("\nAccuracy — TTT: %.2f%% | Bet365: %.2f%% (same %d matches)\n",
            final$ttt_acc_same_odds, final$book_acc_same_odds, final$n_comp))

# Export (tagged)
tag <- sprintf("aScale%.2f", best_a)
tag <- gsub("\\.", "p", tag)

all_df_export <- all_df %>%
  select(-mu_winner, -mu_loser, -sigma_winner, -sigma_loser, -TTT_Prob_W, -TTT_Prob_L)

disagree_df <- all_df %>% filter(bet365_right_ttt_wrong)

write_csv(disagree_df, paste0("bet365_right_ttt_wrong_base_vWeight_", tag, "_GS.csv"))
write.csv(all_df_export, paste0("Global_TTT_rolling_result_base_vWeight_", tag, "_GS.csv"), row.names = FALSE)
write_xlsx(all_df_export, paste0("Global_TTT_rolling_result_base_vWeight_", tag, "_GS.xlsx"))

cat(sprintf("\nFiles written:\n- Global_TTT_rolling_result_base_vWeight_%s_GS.[csv/xlsx]\n- bet365_right_ttt_wrong_base_vWeight_%s_GS.csv\n",
            tag, tag))

cat("\nUnique a_scale in final all_df:\n")
print(unique(all_df$a_scale))
