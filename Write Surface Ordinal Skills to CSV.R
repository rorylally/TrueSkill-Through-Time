# ============================
# SURFACE-NODE ORDINAL TTT (WEIGHTED)
# Rolling eval (2024 R1) + SAVE RESULTS TO CSV
# ============================

# --- Libraries ---
library(readxl)
library(dplyr)
library(purrr)
library(TrueSkillThroughTimeOrdinal)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)

# ----------------------------
# Hyperparameters (match your ordinal script)
# ----------------------------
beta   <- 1
B1     <- 1
B2     <- 1.2
sigma0 <- 4
gamma0 <- 0.006

# ----------------------------
# Helper: null-coalesce + surface tagging
# ----------------------------
`%||%` <- function(a,b) if (is.null(a)) b else a

surf_tag <- function(s) {
  s <- toupper(trimws(s %||% ""))
  if (grepl("CLAY", s))  return("CLAY")
  if (grepl("GRASS", s)) return("GRASS")
  "HARD"
}

# ----------------------------
# OUTPUT FILE
# ----------------------------
OUT_CSV <- "surface_ordinal_ttt_preds_byATPv2.csv"

# ----------------------------
# OPTIONAL: skip model run if CSV already exists
# ----------------------------
if (file.exists(OUT_CSV)) {
  
  message("Found existing CSV: ", OUT_CSV, " — loading instead of re-running model.")
  all_df <- read_csv(OUT_CSV, show_col_types = FALSE)
  
} else {
  
  # ----------------------------
  # Load & prep data (Top-100 + Completed + set-margin)
  # ----------------------------
  df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
    mutate(
      Date    = as.Date(Date),
      Winner  = trimws(Winner),
      Loser   = trimws(Loser),
      Surface = trimws(Surface),
      SurfTag = vapply(Surface, surf_tag, character(1)),
      Comment = trimws(Comment),
      WRank   = suppressWarnings(as.numeric(WRank)),
      LRank   = suppressWarnings(as.numeric(LRank)),
      Wsets   = suppressWarnings(as.integer(Wsets)),
      Lsets   = suppressWarnings(as.integer(Lsets))
    ) %>%
    filter(
      Comment == "Completed",
      !is.na(WRank), !is.na(LRank),
      !is.na(Wsets), !is.na(Lsets)
    ) %>%
    mutate(
      # ordinal weight proxy = set margin, clipped to {1,2,3}
      margin = pmin(pmax(Wsets - Lsets, 1), 3)
    )
  
  # ---- Seed training with 2023 only ----
  train_pool <- df0 %>% filter(year(Date) == 2023)
  
  # ---- All 2024 ATP codes available  ----
  ks <- df0 %>%
    filter(year(Date) == 2024) %>%
    distinct(ATP) %>%
    arrange(ATP) %>%
    pull(ATP)
  
  all_preds <- list()
  
  # ============================
  # Rolling evaluation
  # ============================
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(year(Date) == 2024, ATP == k)
    
    if (nrow(test_df) == 0) next
    
    # --- Build ORDINAL TTT training comps using (player_surface) node id ---
    comp <- purrr::map(seq_len(nrow(train_pool)), function(i) {
      w <- paste0(train_pool$Winner[i], "_", train_pool$SurfTag[i])
      l <- paste0(train_pool$Loser[i],  "_", train_pool$SurfTag[i])
      list(c(w), c(l))
    })
    
    times_tr       <- as.numeric(train_pool$Date)
    set_margins_tr <- train_pool$margin
    
    # ✅ Force the ORDINAL History() (prevents calling the wrong History)
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
    
    # --- Extract latest posterior mu/sigma per node ---
    lc <- m$learning_curves()
    stats_df <- purrr::map_dfr(names(lc), function(id) {
      recs <- lc[[id]]
      last <- recs[[length(recs)]][[2]]
      tibble(id = id, mu = last@mu, sigma = last@sigma)
    })
    
    # --- Ensure all nodes exist (train + test nodes) ---
    all_ids <- unique(c(
      paste0(train_pool$Winner, "_", train_pool$SurfTag),
      paste0(train_pool$Loser,  "_", train_pool$SurfTag),
      paste0(test_df$Winner,    "_", test_df$SurfTag),
      paste0(test_df$Loser,     "_", test_df$SurfTag)
    ))
    
    # For unseen nodes, fall back to prior-ish values
    stats_wide <- tibble(id = all_ids) %>%
      left_join(stats_df, by = "id") %>%
      replace_na(list(mu = 0, sigma = sigma0))
    
    # --- Predict on this tournament's 1R ---
    preds <- test_df %>%
      mutate(
        Winner_id = paste0(Winner, "_", SurfTag),
        Loser_id  = paste0(Loser,  "_", SurfTag)
      ) %>%
      left_join(stats_wide, by = c("Winner_id" = "id")) %>%
      rename(mu_winner = mu, sigma_winner = sigma) %>%
      left_join(stats_wide, by = c("Loser_id" = "id")) %>%
      rename(mu_loser = mu, sigma_loser = sigma) %>%
      mutate(
        .den_raw  = sqrt(pmax(sigma_winner^2 + sigma_loser^2, 0)),
        .den      = if_else(.den_raw > 0, .den_raw, 1e-9),
        TTT_Prob_W = pnorm((mu_winner - mu_loser) / .den),
        TTT_Pct_W  = round(TTT_Prob_W * 100, 2),
        pred        = if_else(mu_winner >= mu_loser, Winner, Loser),
        ttt_correct = pred == Winner,
        
        bookie_pred = case_when(
          !is.na(B365W) & !is.na(B365L) & B365W < B365L ~ Winner,
          !is.na(B365W) & !is.na(B365L) & B365W > B365L ~ Loser,
          TRUE ~ NA_character_
        ),
        bookie_correct = !is.na(bookie_pred) & bookie_pred == Winner,
        bet365_right_ttt_wrong = bookie_correct & !ttt_correct,
        ATPk = k
      )
    
    all_preds[[length(all_preds) + 1]] <- preds
    
    # expand training pool exactly like your pipeline
    train_pool <- bind_rows(train_pool, test_df)
  }
  
  # ---- aggregate + save ----
  all_df <- bind_rows(all_preds)
  write_csv(all_df, OUT_CSV)
  message("Saved rolling predictions to: ", OUT_CSV)
}

# ============================
# ACCURACY REPORT (from saved or freshly-built all_df)
# ============================
comp_df <- all_df %>%
  filter(!is.na(B365W), !is.na(B365L), B365W > 0, B365L > 0, B365W != B365L)

ttt_acc  <- mean(comp_df$ttt_correct, na.rm = TRUE) * 100
book_acc <- mean(comp_df$bookie_correct, na.rm = TRUE) * 100
n_comp <- nrow(comp_df)

cat(sprintf("Accuracy — TTT: %.2f%% | Bet365: %.2f%% (same %d matches)\n",
            ttt_acc, book_acc, n_comp))

cat("Saved: ", OUT_CSV, "\n", sep = "")
