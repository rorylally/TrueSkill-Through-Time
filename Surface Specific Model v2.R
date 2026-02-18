#[All_2024_R1]
#TTT (from CSV skills)
#Accuracy — TTT: 63.82% | Book: 69.11% (same 2609 matches)
#Brier — TTT: 0.2771 | Book: 0.1990
#LogLoss — TTT: 1.0621 | Book: 0.5810
#CalRatio — TTT: 0.6179 | Book: 0.5957

#[ATP250]
#TTT (from CSV skills)
#Accuracy — TTT: 60.79% | Book: 66.12% (same 1033 matches)
#Brier — TTT: 0.2978 | Book: 0.2141
#LogLoss — TTT: 1.0663 | Book: 0.6166
#CalRatio — TTT: 0.5790 | Book: 0.5662

#[ATP500]
#TTT (from CSV skills)
#Accuracy — TTT: 64.18% | Book: 67.53% (same 388 matches)
#Brier — TTT: 0.2829 | Book: 0.2018
#LogLoss — TTT: 1.0755 | Book: 0.5875
#CalRatio — TTT: 0.6219 | Book: 0.5902

#[Masters1000]
#TTT (from CSV skills)
#Accuracy — TTT: 62.54% | Book: 67.85% (same 622 matches)
#Brier — TTT: 0.2797 | Book: 0.2056
#LogLoss — TTT: 1.1339 | Book: 0.5967
#CalRatio — TTT: 0.6256 | Book: 0.5898

#[Grandslams]
#TTT (from CSV skills)
#Accuracy — TTT: 72.60% | Book: 78.40% (same 500 matches)
#Brier — TTT: 0.2171 | Book: 0.1539
#LogLoss — TTT: 0.8618 | Book: 0.4731
#CalRatio — TTT: 0.6899 | Book: 0.6719

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
PRED_CSV <- "surface_ttt_preds_byATPv2.csv"  # <--- change to your filename
stopifnot(file.exists(PRED_CSV))

all_df <- read_csv(PRED_CSV, show_col_types = FALSE) %>%
  mutate(
    ATP  = suppressWarnings(as.integer(ATP)),
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

metrics_block <- function(df) {
  # common "same matches" filter (must have valid odds, and non-equal)
  comp <- df %>%
    filter(!is.na(B365W), !is.na(B365L), B365W > 0, B365L > 0, B365W != B365L) %>%
    mutate(
      p_ttt  = clip01(as.numeric(TTT_Prob_W)),
      p_book = clip01(book_p_winner(B365W, B365L))
    )
  
  if (nrow(comp) == 0) {
    return(list(n=0, ttt=list(acc=NA,brier=NA,logloss=NA,cal=NA),
                book=list(acc=NA,brier=NA,logloss=NA,cal=NA)))
  }
  
  # y is always 1 because each row is the *actual winner*
  # so: Brier = (1 - p)^2, LogLoss = -log(p), CalRatio = mean(p)
  ttt_acc  <- mean(comp$ttt_correct, na.rm = TRUE) * 100
  book_acc <- mean(comp$B365W < comp$B365L, na.rm = TRUE) * 100
  
  ttt_brier  <- mean((1 - comp$p_ttt)^2,  na.rm = TRUE)
  book_brier <- mean((1 - comp$p_book)^2, na.rm = TRUE)
  
  ttt_ll  <- mean(-log(comp$p_ttt),  na.rm = TRUE)
  book_ll <- mean(-log(comp$p_book), na.rm = TRUE)
  
  ttt_cal  <- mean(comp$p_ttt,  na.rm = TRUE)
  book_cal <- mean(comp$p_book, na.rm = TRUE)
  
  list(
    n = nrow(comp),
    ttt  = list(acc=ttt_acc,  brier=ttt_brier,  logloss=ttt_ll,  cal=ttt_cal),
    book = list(acc=book_acc, brier=book_brier, logloss=book_ll, cal=book_cal)
  )
}

print_block <- function(name, out) {
  cat(sprintf("#[%s]\n", name))
  cat("TTT (from CSV skills)\n")
  
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


