#[All_2024_R1]
#TTT (from CSV skills) — MARGIN-WEIGHTED METRICS
#Accuracy — TTT: 67.24% | Book: 71.84% (same 2514 matches)
#Brier — TTT: 0.2292 | Book: 0.1865
#LogLoss — TTT: 0.7042 | Book: 0.5519
#CalRatio — TTT: 0.6202 | Book: 0.6128

#[ATP250]
#TTT (from CSV skills) — MARGIN-WEIGHTED METRICS
#Accuracy — TTT: 62.03% | Book: 67.93% (same 1003 matches)
#Brier — TTT: 0.2530 | Book: 0.2076
#LogLoss — TTT: 0.7466 | Book: 0.6022
#CalRatio — TTT: 0.5738 | Book: 0.5740

#[ATP500]
#TTT (from CSV skills) — MARGIN-WEIGHTED METRICS
#Accuracy — TTT: 67.75% | Book: 70.31% (same 363 matches)
#Brier — TTT: 0.2289 | Book: 0.1885
#LogLoss — TTT: 0.6866 | Book: 0.5566
#CalRatio — TTT: 0.6314 | Book: 0.6059

#[Masters1000]
#TTT (from CSV skills) — MARGIN-WEIGHTED METRICS
#Accuracy — TTT: 67.20% | Book: 70.78% (same 608 matches)
#Brier — TTT: 0.2329 | Book: 0.1941
#LogLoss — TTT: 0.7298 | Book: 0.5700
#CalRatio — TTT: 0.6280 | Book: 0.6032

#[Grandslams]
#TTT (from CSV skills) — MARGIN-WEIGHTED METRICS
#Accuracy — TTT: 75.77% | Book: 80.02% (same 477 matches)
#Brier — TTT: 0.1832 | Book: 0.1417
#LogLoss — TTT: 0.5973 | Book: 0.4440
#CalRatio — TTT: 0.6814 | Book: 0.6907

library(dplyr)
library(readr)
library(stringr)

# ----------------------------
# 0) Tournament sets (your input)
# ----------------------------
TOURNAMENT_SETS <- list(
  All_2024_R1   = NULL,
  ATP250        = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27, 30, 31, 33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53, 57, 58, 59, 63, 64),
  ATP500        = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60, 61),
  Masters1000   = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams    = c(5, 32, 39, 51)
)

# ----------------------------
# 1) Load your saved predictions CSV
# ----------------------------
PRED_CSV <- "surface_ordinal_ttt_preds_byATPv2.csv"  # <--- change to your filename
stopifnot(file.exists(PRED_CSV))

all_df <- read_csv(PRED_CSV, show_col_types = FALSE) %>%
  mutate(
    ATP   = suppressWarnings(as.integer(ATP)),
    B365W = suppressWarnings(as.numeric(B365W)),
    B365L = suppressWarnings(as.numeric(B365L))
  )

# ----------------------------
# 2) Ensure we have TTT probability of the *actual winner*
# ----------------------------
if (!("TTT_Prob_W" %in% names(all_df))) {
  if ("TTT_Pct_W" %in% names(all_df)) {
    all_df <- all_df %>% mutate(TTT_Prob_W = as.numeric(TTT_Pct_W) / 100)
  } else if (all(c("mu_winner","mu_loser","sigma_winner","sigma_loser") %in% names(all_df))) {
    all_df <- all_df %>%
      mutate(
        .den_raw = sqrt(pmax(sigma_winner^2 + sigma_loser^2, 0)),
        .den     = if_else(.den_raw > 0, .den_raw, 1e-9),
        TTT_Prob_W = pnorm((mu_winner - mu_loser) / .den)
      )
  } else {
    stop("CSV must contain TTT_Prob_W (preferred) OR TTT_Pct_W OR (mu/sigma columns) to compute it.")
  }
}

# If ttt_correct not present, reconstruct from pred/Winner if possible
if (!("ttt_correct" %in% names(all_df))) {
  if (all(c("pred","Winner") %in% names(all_df))) {
    all_df <- all_df %>% mutate(ttt_correct = (pred == Winner))
  } else {
    # fallback: define "correct" as prob >= 0.5 (since row is actual winner)
    all_df <- all_df %>% mutate(ttt_correct = (TTT_Prob_W >= 0.5))
  }
}

# ----------------------------
# 3) Metric helpers
# ----------------------------
clip01 <- function(p, eps = 1e-15) pmin(pmax(p, eps), 1 - eps)

# Book implied probability of the *actual winner*, vigorish-adjusted
book_p_winner <- function(odds_w, odds_l) {
  iw <- 1 / odds_w
  il <- 1 / odds_l
  iw / (iw + il)
}

# ---- NEW: weighting helpers (margin-weighted scoring) ----
wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# choose weights:
# - if your CSV already contains `margin`, we use it
# - else fallback to 1
get_match_weight <- function(df) {
  if ("margin" %in% names(df)) {
    w <- suppressWarnings(as.numeric(df$margin))
    # safety: if margin has NAs/zeros/negatives, clamp to 1
    w <- ifelse(is.finite(w) & !is.na(w) & w > 0, w, 1)
    return(w)
  }
  rep(1, nrow(df))
}

metrics_block <- function(df) {
  
  comp <- df %>%
    filter(!is.na(B365W), !is.na(B365L), B365W > 0, B365L > 0, B365W != B365L) %>%
    mutate(
      p_ttt  = clip01(as.numeric(TTT_Prob_W)),
      p_book = clip01(book_p_winner(B365W, B365L))
    )
  
  if (nrow(comp) == 0) {
    return(list(
      n = 0,
      ttt  = list(acc=NA,brier=NA,logloss=NA,cal=NA),
      book = list(acc=NA,brier=NA,logloss=NA,cal=NA)
    ))
  }
  
  # NEW: weights
  w <- get_match_weight(comp)
  
  # y is always 1 because each row is the *actual winner*
  # Weighted:
  #   Brier  = weighted mean of (1 - p)^2
  #   LogLoss= weighted mean of -log(p)
  #   Cal    = weighted mean of p
  # Accuracy:
  #   TTT: weighted mean of (ttt_correct)
  #   Book hard-pick: winner is fav if B365W < B365L (because row is winner)
  ttt_acc  <- 100 * wmean(as.numeric(comp$ttt_correct), w)
  book_acc <- 100 * wmean(as.numeric(comp$B365W < comp$B365L), w)
  
  ttt_brier  <- wmean((1 - comp$p_ttt)^2,  w)
  book_brier <- wmean((1 - comp$p_book)^2, w)
  
  ttt_ll  <- wmean(-log(comp$p_ttt),  w)
  book_ll <- wmean(-log(comp$p_book), w)
  
  ttt_cal  <- wmean(comp$p_ttt,  w)
  book_cal <- wmean(comp$p_book, w)
  
  list(
    n = nrow(comp),
    ttt  = list(acc=ttt_acc,  brier=ttt_brier,  logloss=ttt_ll,  cal=ttt_cal),
    book = list(acc=book_acc, brier=book_brier, logloss=book_ll, cal=book_cal)
  )
}

print_block <- function(name, out) {
  cat(sprintf("#[%s]\n", name))
  cat("TTT (from CSV skills) — MARGIN-WEIGHTED METRICS\n")
  
  cat(sprintf("Accuracy — TTT: %.2f%% | Book: %.2f%% (same %d matches)\n",
              out$ttt$acc, out$book$acc, out$n))
  cat(sprintf("Brier — TTT: %.4f | Book: %.4f\n",
              out$ttt$brier, out$book$brier))
  cat(sprintf("LogLoss — TTT: %.4f | Book: %.4f\n",
              out$ttt$logloss, out$book$logloss))
  cat(sprintf("CalRatio — TTT: %.4f | Book: %.4f\n\n",
              out$ttt$cal, out$book$cal))
}

# ----------------------------
# 4) Run for each tournament set + print like your example
# ----------------------------
for (nm in names(TOURNAMENT_SETS)) {
  atps <- TOURNAMENT_SETS[[nm]]
  
  df_sub <- if (is.null(atps)) {
    all_df
  } else {
    all_df %>% filter(ATP %in% atps)
  }
  
  out <- metrics_block(df_sub)
  print_block(nm, out)
}
# ============================
# 5) Surface-wise accuracy
# ============================
SURFACES <- sort(unique(all_df$SurfTag))

cat("\n===== SURFACE-WISE RESULTS =====\n\n")

for (s in SURFACES) {
  
  df_surf <- all_df %>% filter(SurfTag == s)
  
  if (nrow(df_surf) == 0) next
  
  out <- metrics_block(df_surf)
  
  cat(sprintf("[Surface: %s]\n", s))
  cat(sprintf(
    "Accuracy — TTT: %.2f%% | Book: %.2f%% (same %d matches)\n",
    out$ttt$acc, out$book$acc, out$n
  ))
  cat(sprintf(
    "Brier — TTT: %.4f | Book: %.4f\n",
    out$ttt$brier, out$book$brier
  ))
  cat(sprintf(
    "LogLoss — TTT: %.4f | Book: %.4f\n",
    out$ttt$logloss, out$book$logloss
  ))
  cat(sprintf(
    "CalRatio — TTT: %.4f | Book: %.4f\n\n",
    out$ttt$cal, out$book$cal
  ))
}

