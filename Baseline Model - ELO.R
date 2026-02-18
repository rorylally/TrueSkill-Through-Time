#[All_2024_R1]
#Elo rolling (K=32, init=1500)
#Accuracy — Elo: 61.41% | Book: 68.75% (same 1104 matches)
#Brier — Elo: 0.2268 | Book: 0.1989
#LogLoss — Elo: 0.6420 | Book: 0.5788
#CalRatio — Elo: 0.5469 | Book: 0.5913

#[ATP250]                                                                                                     
#Elo rolling (K=32, init=1500)
#Accuracy — Elo: 57.30% | Book: 66.97% (same 445 matches)
#Brier — Elo: 0.2405 | Book: 0.2121
#LogLoss — Elo: 0.6733 | Book: 0.6110
#CalRatio — Elo: 0.5219 | Book: 0.5650

#[ATP500]                                                                                                     
#Elo rolling (K=32, init=1500)
#Accuracy — Elo: 67.24% | Book: 68.39% (same 174 matches)
#Brier — Elo: 0.2173 | Book: 0.1947
#LogLoss — Elo: 0.6227 | Book: 0.5730
#CalRatio — Elo: 0.5667 | Book: 0.5962

#[Masters1000]                                                                                                
#Elo rolling (K=32, init=1500)
#Accuracy — Elo: 58.80% | Book: 65.74% (same 216 matches)
#Brier — Elo: 0.2387 | Book: 0.2137
#LogLoss — Elo: 0.6693 | Book: 0.6143
#CalRatio — Elo: 0.5289 | Book: 0.5655

#[Grandslams]                                                                                                 
#Elo rolling (K=32, init=1500)
#Accuracy — Elo: 68.29% | Book: 76.02% (same 246 matches)
#Brier — Elo: 0.1935 | Book: 0.1585
#LogLoss — Elo: 0.5640 | Book: 0.4785
#CalRatio — Elo: 0.5983 | Book: 0.6651

library(readxl)
library(dplyr)
library(lubridate)
library(stringr)
library(readr)
library(tibble)

# ----------------------------
# 0) Tournament sets
# ----------------------------
TOURNAMENT_SETS <- list(
  All_2024_R1 = NULL,
  ATP250 = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27, 30, 31, 33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53, 57, 58, 59, 63, 64),
  ATP500 = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60, 61),
  Masters1000 = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams = c(5, 32, 39, 51)
)

# ----------------------------
# 1) Metrics helpers
# ----------------------------
brier_score <- function(p, y) mean((p - y)^2, na.rm = TRUE)

log_loss <- function(p, y) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
}

# ----------------------------
# 2) Elo engine (in-memory env)
# ----------------------------
make_elo_engine <- function(init_elo = 1500) {
  eloEnv <- new.env(parent = emptyenv())
  
  getElo <- function(player) {
    if (exists(player, envir = eloEnv, inherits = FALSE)) {
      get(player, envir = eloEnv, inherits = FALSE)
    } else {
      init_elo
    }
  }
  
  setElo <- function(player, val) {
    assign(player, val, envir = eloEnv)
  }
  
  list(getElo = getElo, setElo = setElo)
}

elo_prob <- function(Ew, El, scale = 400) {
  1 / (1 + 10 ^ ((El - Ew) / scale))
}

# ----------------------------
# 3) Load + prep match data
# ----------------------------
df <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date = as.Date(Date),
    Year = year(Date),
    Winner = str_trim(Winner),
    Loser  = str_trim(Loser),
    Round  = str_trim(Round),
    Comment = if ("Comment" %in% names(.)) str_trim(Comment) else NA_character_
  ) %>%
  # if your file has Comment, keep completed only (safer)
  filter(is.na(Comment) | Comment == "Completed") %>%
  arrange(Date)

# Base tournament list present in 2024 1st Round
ks_all <- df %>%
  filter(Year == 2024, Round == "1st Round") %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ----------------------------
# 4) Runner: rolling Elo -> evaluate a tournament set on 2024 R1
# ----------------------------
run_elo_for_set <- function(set_name,
                            atp_vec_or_null,
                            K = 32,
                            init_elo = 1500,
                            elo_scale = 400) {
  
  ks <- if (is.null(atp_vec_or_null)) {
    ks_all
  } else {
    sort(intersect(ks_all, atp_vec_or_null))
  }
  
  eng <- make_elo_engine(init_elo = init_elo)
  getElo <- eng$getElo
  setElo <- eng$setElo
  
  match_list <- list()
  ctr <- 1L
  
  # Roll through ALL matches in chronological order,
  # update Elo after each match,
  # but only RECORD predictions for 2024 1st Round within ks set.
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    
    w <- as.character(row$Winner)
    l <- as.character(row$Loser)
    
    Ew <- getElo(w)
    El <- getElo(l)
    
    p_win <- elo_prob(Ew, El, scale = elo_scale)
    
    # Record ONLY for the chosen evaluation slice:
    if (row$Year == 2024 &&
        row$Round == "1st Round" &&
        (is.null(atp_vec_or_null) || row$ATP %in% ks)) {
      
      # Book implied probability if odds exist (remove overround)
      book_p <- NA_real_
      if (!is.na(row$B365W) && !is.na(row$B365L) && row$B365W > 0 && row$B365L > 0 && row$B365W != row$B365L) {
        book_p <- (1 / row$B365W) / ((1 / row$B365W) + (1 / row$B365L))
      }
      
      match_list[[ctr]] <- tibble(
        idx        = i,
        Date       = row$Date,
        ATP        = row$ATP,
        Tournament = if ("Tournament" %in% names(df)) as.character(row$Tournament) else NA_character_,
        Round      = row$Round,
        Winner     = w,
        Loser      = l,
        
        Elo_w_pre  = Ew,
        Elo_l_pre  = El,
        p_win_elo  = p_win,
        p_lose_elo = 1 - p_win,
        
        B365W      = row$B365W,
        B365L      = row$B365L,
        p_win_book = book_p,
        
        actual     = 1,
        set_name   = set_name
      )
      ctr <- ctr + 1L
    }
    
    # Update Elo after match (standard)
    newEw <- Ew + K * (1 - p_win)
    newEl <- El + K * (0 - (1 - p_win))  # equivalent to El - K*(1 - p_win)
    # clearer: loser loses K*(1 - p_win)
    newEl <- El - K * (1 - p_win)
    
    setElo(w, newEw)
    setElo(l, newEl)
  }
  
  preds <- bind_rows(match_list)
  
  # Keep same-match evaluation: only where book odds are usable (like your baseline)
  comp <- preds %>%
    filter(!is.na(B365W), !is.na(B365L),
           B365W > 0, B365L > 0,
           B365W != B365L,
           !is.na(p_win_book))
  
  # Elo metrics
  elo_acc   <- mean((comp$p_win_elo >= 0.5) == (comp$actual == 1))
  elo_brier <- brier_score(comp$p_win_elo, comp$actual)
  elo_ll    <- log_loss(comp$p_win_elo, comp$actual)
  elo_cal_ratio <- sum(comp$p_win_elo) / sum(comp$actual)
  
  # Book metrics (for comparison)
  book_acc   <- mean((comp$p_win_book >= 0.5) == (comp$actual == 1))
  book_brier <- brier_score(comp$p_win_book, comp$actual)
  book_ll    <- log_loss(comp$p_win_book, comp$actual)
  book_cal_ratio <- sum(comp$p_win_book) / sum(comp$actual)
  
  n_comp <- nrow(comp)
  
  cat(sprintf(
    "[%s]\nElo rolling (K=%d, init=%d)\nAccuracy — Elo: %.2f%% | Book: %.2f%% (same %d matches)\nBrier — Elo: %.4f | Book: %.4f\nLogLoss — Elo: %.4f | Book: %.4f\nCalRatio — Elo: %.4f | Book: %.4f\n\n",
    set_name, K, init_elo,
    100*elo_acc, 100*book_acc, n_comp,
    elo_brier, book_brier,
    elo_ll, book_ll,
    elo_cal_ratio, book_cal_ratio
  ))
  
  # Exports
  preds_path   <- sprintf("Elo_rolling_predictions_%s.csv", set_name)
  summary_path <- sprintf("metrics_summary_ELO_ROLLING_%s.csv", set_name)
  
  write_csv(preds, preds_path)
  
  summary_df <- tibble(
    tournament_set = set_name,
    K = K,
    init_elo = init_elo,
    elo_scale = elo_scale,
    n_tournaments_used = length(ks),
    n_matches = n_comp,
    
    acc_elo  = round(100*elo_acc, 2),
    acc_book = round(100*book_acc, 2),
    
    brier_elo  = round(elo_brier, 4),
    brier_book = round(book_brier, 4),
    
    logloss_elo  = round(elo_ll, 4),
    logloss_book = round(book_ll, 4),
    
    calratio_elo  = round(elo_cal_ratio, 4),
    calratio_book = round(book_cal_ratio, 4)
  )
  write_csv(summary_df, summary_path)
  
  list(
    set_name = set_name,
    n_comp = n_comp,
    acc_elo = 100*elo_acc,
    brier_elo = elo_brier,
    logloss_elo = elo_ll,
    calratio_elo = elo_cal_ratio,
    acc_book = 100*book_acc,
    brier_book = book_brier,
    logloss_book = book_ll,
    calratio_book = book_cal_ratio,
    preds_path = preds_path,
    summary_path = summary_path
  )
}

# ----------------------------
# 5) Run all sets + combined summary
# ----------------------------
results <- lapply(names(TOURNAMENT_SETS), function(nm) {
  run_elo_for_set(
    set_name = nm,
    atp_vec_or_null = TOURNAMENT_SETS[[nm]],
    K = 32,
    init_elo = 1500,
    elo_scale = 400
  )
})

results_df <- bind_rows(lapply(results, function(x) {
  tibble(
    tournament_set = x$set_name,
    n_matches = x$n_comp,
    acc_elo = round(x$acc_elo, 2),
    acc_book = round(x$acc_book, 2),
    brier_elo = round(x$brier_elo, 4),
    brier_book = round(x$brier_book, 4),
    logloss_elo = round(x$logloss_elo, 4),
    logloss_book = round(x$logloss_book, 4),
    calratio_elo = round(x$calratio_elo, 4),
    calratio_book = round(x$calratio_book, 4),
    preds_file = x$preds_path,
    summary_file = x$summary_path
  )
}))


