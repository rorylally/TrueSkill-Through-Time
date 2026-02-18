# ==========================================================
# TIER-A GRID SEARCH (TRAIN 2023, TEST 2024) + CSV OUTPUTS
# Tunes ONLY Tier A = MOV_SCALE for ACCURACY
# Uses your MOV-enabled History() from: ttt_src/TrueSkillThroughTimeMOV
#
# Writes:
#  (1) tierA_grid_results.csv        (one row per MOV_SCALE)
#  (2) OPTIONAL per-tier skill snapshots:
#      skills_snapshot_MOVSCALE_<value>.csv  (ATP | player | mu_generic | sigma_generic)
#
# Notes:
# - Uses 2024 1st Round tournaments (like your pipeline).
# - Evaluation uses SAME matches with usable odds (B365W/B365L), and predicts
#   winner vs loser as in your earlier style:
#     predict Winner if p_win >= 0.5, else predict Loser
# - Weighting (MOV) is applied in training via mov_weights_in per match.
# ==========================================================

library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(readr)
library(pkgload)

# ----------------------------
# 0) LOAD YOUR MOV PACKAGE (NO devtools needed)
# ----------------------------
pkgload::load_all("ttt_src/TrueSkillThroughTimeMOV")

# ----------------------------
# 1) SETTINGS
# ----------------------------
DATA_FILE <- "2023_2024_ATP_Results.xlsx"

# Tier A grid (MOV_SCALE)
TIER_A_GRID <- seq(0.05, 0.50, by = 0.05)

# Keep cap fixed
MOV_CAP_FIXED <- 2.50

# Convergence (speed)
EPS_CONV  <- 0.01
ITER_CONV <- 6

# If TRUE, write big per-tier snapshot file (slower)
WRITE_SNAPSHOTS_PER_TIERA <- FALSE

# Optional: tournament sets (matches your style)
TOURNAMENT_SETS <- list(
  All_2024_R1 = NULL,
  ATP250 = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27, 30, 31,
             33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53, 57, 58, 59, 63, 64),
  ATP500 = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60, 61),
  Masters1000 = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams = c(5, 32, 39, 51)
)

# ----------------------------
# 2) HELPERS
# ----------------------------

# MOV weight from margin (sets margin here), capped
# returns weights w >= 1
mov_weight_from_margin <- function(margin_sets, mov_scale, mov_cap) {
  m <- mov_scale * sqrt(pmax(margin_sets, 1))
  m <- pmin(m, mov_cap)
  w <- 1 + m
  w[!is.finite(w) | w <= 0] <- 1
  w
}

# Build TTT inputs for 1v1 from a df with Winner/Loser/Date
build_ttt_inputs <- function(df) {
  comp <- map(seq_len(nrow(df)), function(i) {
    list(c(df$Winner[i]), c(df$Loser[i]))
  })
  res <- map(seq_len(nrow(df)), function(i) c(1, 0))
  times <- as.numeric(df$Date)
  list(comp = comp, res = res, times = times)
}

# Convert learning curves hash -> tibble(player, mu, sigma) (latest)
lc_to_skills_df <- function(lc_hash) {
  ids <- names(lc_hash)
  map_dfr(ids, function(id) {
    last <- lc_hash[[id]][[length(lc_hash[[id]])]][[2]]
    tibble(player = id, mu = last@mu, sigma = last@sigma)
  })
}

# Ordinal-consistent win probability
p_win_from_skills <- function(mu_w, sig_w, mu_l, sig_l, beta = BETA) {
  denom <- sqrt(pmax(2 * beta^2 + sig_w^2 + sig_l^2, 1e-9))
  z <- (mu_w - mu_l) / denom
  pnorm(z)
}

# Evaluate accuracy on same rows (Winner/Loser orientation)
# - Predict Winner if p_win >= 0.5 else predict Loser
evaluate_accuracy <- function(test_df, skills_df) {
  preds <- test_df %>%
    left_join(skills_df, by = c("Winner" = "player")) %>%
    left_join(skills_df, by = c("Loser"  = "player"), suffix = c("_w", "_l")) %>%
    mutate(
      mu_w = coalesce(mu_w, 0),
      mu_l = coalesce(mu_l, 0),
      sg_w = coalesce(sigma_w, SIGMA),
      sg_l = coalesce(sigma_l, SIGMA),
      p_win = p_win_from_skills(mu_w, sg_w, mu_l, sg_l, beta = BETA),
      pred = if_else(p_win >= 0.5, Winner, Loser),
      correct = (pred == Winner)
    )
  mean(preds$correct, na.rm = TRUE)
}

# ----------------------------
# 3) LOAD + PREP DATA (USING YOUR REAL COLUMN NAMES)
# ----------------------------
df0 <- read_excel(DATA_FILE)

# IMPORTANT: remove hidden spaces in column names like "Date "
names(df0) <- trimws(names(df0))

df0 <- df0 %>%
  mutate(
    Date    = as.Date(.data$Date),
    Year    = lubridate::year(.data$Date),
    Winner  = trimws(.data$Winner),
    Loser   = trimws(.data$Loser),
    Round   = trimws(.data$Round),
    Comment = trimws(.data$Comment),
    WRank   = suppressWarnings(as.numeric(.data$WRank)),
    LRank   = suppressWarnings(as.numeric(.data$LRank)),
    Wsets   = suppressWarnings(as.integer(.data$Wsets)),
    Lsets   = suppressWarnings(as.integer(.data$Lsets)),
    ATP     = suppressWarnings(as.integer(.data$ATP)),
    margin_sets = pmax(1L, .data$Wsets - .data$Lsets)
  ) %>%
  filter(
    .data$Comment == "Completed",
    !is.na(.data$Date),
    !is.na(.data$Winner), !is.na(.data$Loser)
  )
%>%
  mutate(
    margin_sets = pmax(1L, Wsets - Lsets)
  )

# Training = all 2023
train_all_2023 <- df0 %>% filter(Year == 2023)

# Base tournament list present in 2024 1st Round
ks_all <- df0 %>%
  filter(Year == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ----------------------------
# 4) RUN ONE TOURNAMENT SET FOR ONE MOV_SCALE
# ----------------------------
run_one_set_one_tierA <- function(set_name, atp_vec_or_null, mov_scale) {
  
  ks <- if (is.null(atp_vec_or_null)) {
    ks_all
  } else {
    sort(intersect(ks_all, atp_vec_or_null))
  }
  
  train_pool <- train_all_2023
  
  # Collect accuracy contributions over tournaments
  acc_by_k <- list()
  ctr <- 1L
  
  # Optional: collect snapshots
  snapshots <- list()
  snap_ctr <- 1L
  
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(Year == 2024, ATP == k, Round == "1st Round") %>%
      # require usable odds so evaluation is comparable like your other scripts
      filter(!is.na(B365W), !is.na(B365L), B365W > 0, B365L > 0, B365W != B365L)
    
    if (nrow(test_df) == 0) {
      # still expand train_pool in exact pipeline? you used: expand by test_df
      # but if test_df empty, nothing to add
      next
    }
    
    # --- Train model on current train_pool with MOV weights ---
    tr_inputs <- build_ttt_inputs(train_pool)
    
    # MOV weights from sets margin in TRAINING DATA
    w_tr <- mov_weight_from_margin(train_pool$margin_sets, mov_scale, MOV_CAP_FIXED)
    
    m <- History(
      composition     = tr_inputs$comp,
      results         = tr_inputs$res,
      times           = tr_inputs$times,
      sigma           = SIGMA,     # keep your package defaults unless you changed
      gamma           = GAMMA,
      beta            = BETA,
      p_draw          = P_DRAW,
      mov_weights_in  = w_tr
    )
    m$convergence(epsilon = EPS_CONV, iterations = ITER_CONV, verbose = FALSE)
    
    # Snapshot skills
    skills_df <- lc_to_skills_df(m$learning_curves())
    
    # Evaluate on this tournament
    acc_k <- evaluate_accuracy(test_df, skills_df)
    acc_by_k[[ctr]] <- tibble(
      tournament_set = set_name,
      ATP            = k,
      mov_scale      = mov_scale,
      n_matches      = nrow(test_df),
      acc            = acc_k
    )
    ctr <- ctr + 1L
    
    # Optional snapshot output (ATP-tagged)
    if (isTRUE(WRITE_SNAPSHOTS_PER_TIERA)) {
      all_players <- unique(c(train_pool$Winner, train_pool$Loser, test_df$Winner, test_df$Loser))
      stats_wide <- tibble(player = all_players) %>%
        left_join(skills_df, by = "player") %>%
        replace_na(list(mu = 0, sigma = SIGMA)) %>%
        transmute(ATP = k, player, mu_generic = mu, sigma_generic = sigma)
      
      snapshots[[snap_ctr]] <- stats_wide
      snap_ctr <- snap_ctr + 1L
    }
    
    # Expand training pool exactly like your pipeline
    train_pool <- bind_rows(train_pool, test_df)
  }
  
  acc_df <- bind_rows(acc_by_k)
  
  # Weighted-by-matches overall accuracy across tournaments in this set
  overall_acc <- with(acc_df, sum(acc * n_matches, na.rm = TRUE) / sum(n_matches, na.rm = TRUE))
  
  # Write optional snapshots per tierA
  snapshot_file <- NA_character_
  if (isTRUE(WRITE_SNAPSHOTS_PER_TIERA) && length(snapshots) > 0) {
    snapshot_file <- sprintf("skills_snapshot_MOVSCALE_%0.2f_%s.csv", mov_scale, set_name)
    write_csv(bind_rows(snapshots), snapshot_file)
  }
  
  list(
    set_name = set_name,
    mov_scale = mov_scale,
    n_tournaments_used = n_distinct(acc_df$ATP),
    n_matches = sum(acc_df$n_matches, na.rm = TRUE),
    acc = overall_acc,
    snapshot_file = snapshot_file
  )
}

# ----------------------------
# 5) GRID SEARCH OVER MOV_SCALE
# ----------------------------
grid_results <- list()
gctr <- 1L

for (a in TIER_A_GRID) {
  
  cat(sprintf("\n=== Testing MOV_SCALE (Tier A) = %.2f ===\n", a))
  
  # Run all tournament sets (or comment out if you only want All_2024_R1)
  res_list <- lapply(names(TOURNAMENT_SETS), function(nm) {
    run_one_set_one_tierA(nm, TOURNAMENT_SETS[[nm]], mov_scale = a)
  })
  
  res_df <- bind_rows(lapply(res_list, function(x) {
    tibble(
      tournament_set     = x$set_name,
      mov_scale          = x$mov_scale,
      n_tournaments_used = x$n_tournaments_used,
      n_matches          = x$n_matches,
      acc                = round(100 * x$acc, 4),
      snapshot_file      = x$snapshot_file
    )
  }))
  
  # Save per-tier detailed set results (optional but useful)
  per_tier_file <- sprintf("tierA_detail_MOVSCALE_%0.2f.csv", a)
  write_csv(res_df, per_tier_file)
  
  # Main score to optimize:
  # choose All_2024_R1 if present, else weighted avg across sets
  if ("All_2024_R1" %in% res_df$tournament_set) {
    main_acc <- res_df %>% filter(tournament_set == "All_2024_R1") %>% pull(acc)
    main_n   <- res_df %>% filter(tournament_set == "All_2024_R1") %>% pull(n_matches)
  } else {
    main_acc <- sum(res_df$acc * res_df$n_matches) / sum(res_df$n_matches)
    main_n   <- sum(res_df$n_matches)
  }
  
  grid_results[[gctr]] <- tibble(
    mov_scale = a,
    mov_cap   = MOV_CAP_FIXED,
    eps_conv  = EPS_CONV,
    iter_conv = ITER_CONV,
    main_set  = if ("All_2024_R1" %in% res_df$tournament_set) "All_2024_R1" else "ALL_SETS_WEIGHTED",
    n_matches = as.integer(main_n),
    acc       = as.numeric(main_acc),
    detail_file = per_tier_file
  )
  gctr <- gctr + 1L
  
  cat(sprintf("Finished MOV_SCALE = %.2f | Accuracy = %.4f%% | n = %d\n", a, main_acc, main_n))
}

grid_df <- bind_rows(grid_results) %>%
  arrange(desc(acc))

write_csv(grid_df, "tierA_grid_results.csv")

cat("\nDONE.\nWrote: tierA_grid_results.csv\n")
cat("Per-tier details: tierA_detail_MOVSCALE_<value>.csv\n")
if (WRITE_SNAPSHOTS_PER_TIERA) {
  cat("Optional snapshots: skills_snapshot_MOVSCALE_<value>_<SET>.csv\n")
}

