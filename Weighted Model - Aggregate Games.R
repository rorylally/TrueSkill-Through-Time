
#Fine-tuned parameters Top100
#tournament_set n_tournaments_used n_matches acc_ttt acc_book brier_ttt brier_book logloss_ttt logloss_book
#<chr>                       <int>     <int>   <dbl>    <dbl>     <dbl>      <dbl>       <dbl>        <dbl>
#1 Grandslams                      4       327    73.4     79.2     0.234      0.152        2.54        0.471
#2 Masters1000                     8       479    64.9     68.3     0.312      0.204        3.50        0.591
#3 All_2024_R1                    65      1680    64.0     68.2     0.312      0.201        3.10        0.585
#4 ATP500                         12       289    60.9     64.4     0.335      0.211        3.37        0.605
#5 ATP250                         39       524    60.5     63.6     0.338      0.220        2.71        0.631


#tournament_set n_tournaments_used n_matches acc_ttt acc_book brier_ttt brier_book logloss_ttt logloss_book
#<chr>                       <int>     <int>   <dbl>    <dbl>     <dbl>      <dbl>       <dbl>        <dbl>
#1 Grandslams                      4       500    72.2     78.4     0.246      0.154        2.37        0.473
#2 Masters1000                     8       622    64.6     67.8     0.314      0.206        3.36        0.597
#3 All_2024_R1                    65      2609    64.4     69.1     0.311      0.199        2.79        0.581
#4 ATP500                         12       388    63.1     67.5     0.315      0.202        2.94        0.588
#5 ATP250                         39      1033    61.8     66.1     0.332      0.214        2.47        0.617



#Top-100 Results
#tournament_set n_tournaments_used n_matches acc_ttt acc_book brier_ttt brier_book logloss_ttt logloss_book
#<chr>                       <int>     <int>   <dbl>    <dbl>     <dbl>      <dbl>       <dbl>        <dbl>
#1 Grandslams                      4       132    75       76.5     0.203      0.152       0.898        0.460
#2 ATP500                         12       122    63.1     62.3     0.295      0.212       1.55         0.612
#3 All_2024_R1                    64       593    62.7     65.6     0.294      0.207       1.23         0.596
#4 Masters1000                     8       140    59.3     64.3     0.323      0.221       1.27         0.631
#5 ATP250                         39       179    56.4     62.0     0.333      0.226       1.17         0.643


library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(tibble)

# ----------------------------
# 0) Tournament sets (your input)
# ----------------------------
TOURNAMENT_SETS <- list(
  All_2024_R1 = NULL,
  ATP250 = c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 18, 21, 22, 23, 26, 27, 30, 31, 33, 34, 37, 38, 40, 41, 42, 43, 44, 45, 46, 50, 52, 53, 57, 58, 59, 63, 64),
  ATP500 = c(12, 15, 16, 17, 25, 35, 36, 47, 54, 55, 60, 61),
  Masters1000 = c(19, 20, 24, 28, 29, 48, 56, 62),
  Grandslams = c(5, 32, 39, 51)
)

# ----------------------------
# 1) Proper scoring rules
# ----------------------------
brier_score <- function(p, y) mean((p - y)^2, na.rm = TRUE)

log_loss <- function(p, y) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
}

# ----------------------------
# 2) MOV weighting for "large wins count more"
# ----------------------------
# ---- Vectorised MOV weight (works inside mutate) ----
MOV_PRED_SCALE <- 0.35
MOV_PRED_CAP   <- 4.0

mov_weight_from_games <- function(gw, gl){
  # gw, gl can be vectors
  ok <- is.finite(gw) & is.finite(gl)
  
  m <- pmax(gw - gl, 0)
  w <- 1.0 + MOV_PRED_SCALE * sqrt(m)
  w <- pmin(w, MOV_PRED_CAP)
  
  # default weight = 1 where missing
  w[!ok] <- 1.0
  w
}


# compute total games from W1..W5 / L1..L5 (NA-safe)
total_games_from_sets <- function(df, w_prefix = "W", l_prefix = "L", max_sets = 5L){
  w_cols <- paste0(w_prefix, seq_len(max_sets))
  l_cols <- paste0(l_prefix, seq_len(max_sets))
  
  for (cc in c(w_cols, l_cols)) {
    df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
  }
  gw <- rowSums(df[w_cols], na.rm = TRUE)
  gl <- rowSums(df[l_cols], na.rm = TRUE)
  list(gw = gw, gl = gl)
}

# ----------------------------
# 3) Load data
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Year    = year(Date),
    Winner  = str_trim(Winner),
    Loser   = str_trim(Loser),
    Surface = str_trim(Surface),
    Court   = str_trim(Court),
    Round   = str_trim(Round),
    WRank = suppressWarnings(as.numeric(WRank)),
    LRank = suppressWarnings(as.numeric(LRank))
  )

# add total games + per-match weight
tg <- total_games_from_sets(df0, w_prefix = "W", l_prefix = "L", max_sets = 5L)
df0 <- df0 %>%
  mutate(
    games_w = tg$gw,
    games_l = tg$gl,
    w_event = mov_weight_from_games(games_w, games_l)
  )

skills <- read_csv("skills_snapshot_by_ATP_best_MOVv2.csv", show_col_types = FALSE) %>%
  mutate(player = str_trim(player))

# ----------------------------
# 4) Base tournament list available in 2024 R1
# ----------------------------
ks_all <- df0 %>%
  filter(Year == 2024) %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ----------------------------
# 5) Runner for one set of tournaments (TTT snapshots)
# ----------------------------
run_ttt_for_set <- function(set_name, atp_vec_or_null) {
  
  ks <- if (is.null(atp_vec_or_null)) {
    ks_all
  } else {
    sort(intersect(ks_all, atp_vec_or_null))
  }
  
  match_list <- list()
  ctr <- 1L
  
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(Year == 2024, ATP == k)
    if (nrow(test_df) == 0) next
    
    stats_wide <- skills %>%
      filter(ATP == k) %>%
      select(player, mu_generic, sigma_generic)
    
    preds <- test_df %>%
      left_join(stats_wide, by = c("Winner" = "player")) %>%
      left_join(stats_wide, by = c("Loser"  = "player"), suffix = c("_w","_l")) %>%
      mutate(
        mu_winner    = coalesce(mu_generic_w, 0),
        mu_loser     = coalesce(mu_generic_l, 0),
        sigma_winner = coalesce(sigma_generic_w, 4),
        sigma_loser  = coalesce(sigma_generic_l, 4),
        
        # -------------------------------------------------------
        # TTT probability WITH large-wins scaling:
        # scale the skill gap by sqrt(w_event)
        # (equivalent to shrinking beta by 1/sqrt(w) in prediction space)
        # -------------------------------------------------------
        z_val = ((mu_winner - mu_loser) * sqrt(w_event)) /
          sqrt(pmax(sigma_winner^2 + sigma_loser^2, 1e-9)),
        p_win_ttt  = pnorm(z_val),
        p_lose_ttt = 1 - p_win_ttt,
        
        ttt_pred = if_else(p_win_ttt >= 0.5, Winner, Loser),
        ttt_correct = (ttt_pred == Winner),
        
        # Bookmaker implied prob (normalized overround)
        p_win_book = if_else(
          !is.na(B365W) & !is.na(B365L) & B365W > 0 & B365L > 0 & B365W != B365L,
          (1 / B365W) / ((1 / B365W) + (1 / B365L)),
          NA_real_
        ),
        p_lose_book = if_else(!is.na(p_win_book), 1 - p_win_book, NA_real_),
        
        book_pred = case_when(
          !is.na(B365W) & !is.na(B365L) & B365W < B365L ~ Winner,
          !is.na(B365W) & !is.na(B365L) & B365W > B365L ~ Loser,
          TRUE ~ NA_character_
        ),
        book_correct = !is.na(book_pred) & (book_pred == Winner),
        
        actual = 1,
        set_name = set_name,
        ATPk = k
      )
    
    match_list[[ctr]] <- preds %>%
      transmute(
        Date,
        ATP,
        Tournament = if ("Tournament" %in% names(df0)) as.character(Tournament) else NA_character_,
        Round,
        Winner,
        Loser,
        games_w, games_l, w_event,
        mu_winner, mu_loser,
        sigma_winner, sigma_loser,
        z_val,
        p_win_ttt, p_lose_ttt,
        p_win_book, p_lose_book,
        ttt_correct, book_correct,
        actual,
        set_name,
        ATPk,
        B365W, B365L
      )
    ctr <- ctr + 1L
  }
  
  all_preds <- bind_rows(match_list)
  
  comp <- all_preds %>%
    filter(
      !is.na(B365W), !is.na(B365L),
      B365W > 0, B365L > 0,
      B365W != B365L,
      !is.na(p_win_book),
      !is.na(p_win_ttt)
    )
  
  n_comp <- nrow(comp)
  
  acc_ttt   <- mean((comp$p_win_ttt >= 0.5) == (comp$actual == 1))
  brier_ttt <- brier_score(comp$p_win_ttt, comp$actual)
  ll_ttt    <- log_loss(comp$p_win_ttt, comp$actual)
  cal_ttt   <- sum(comp$p_win_ttt) / sum(comp$actual)
  
  acc_book   <- mean((comp$p_win_book >= 0.5) == (comp$actual == 1))
  brier_book <- brier_score(comp$p_win_book, comp$actual)
  ll_book    <- log_loss(comp$p_win_book, comp$actual)
  cal_book   <- sum(comp$p_win_book) / sum(comp$actual)
  
  cat(sprintf(
    "[%s]\nTTT (from CSV skills) [Large wins scaled]\nAccuracy — TTT: %.2f%% | Book: %.2f%% (same %d matches)\nBrier — TTT: %.4f | Book: %.4f\nLogLoss — TTT: %.4f | Book: %.4f\nCalRatio — TTT: %.4f | Book: %.4f\n\n",
    set_name,
    100*acc_ttt, 100*acc_book, n_comp,
    brier_ttt, brier_book,
    ll_ttt, ll_book,
    cal_ttt, cal_book
  ))
  
  preds_path   <- sprintf("TTT_predictions_%s_largewins.csv", set_name)
  summary_path <- sprintf("metrics_summary_TTT_%s_largewins.csv", set_name)
  
  write_csv(all_preds, preds_path)
  
  summary_df <- tibble(
    tournament_set = set_name,
    n_tournaments_used = length(ks),
    n_matches = n_comp,
    
    acc_ttt  = round(100*acc_ttt, 2),
    acc_book = round(100*acc_book, 2),
    
    brier_ttt  = round(brier_ttt, 4),
    brier_book = round(brier_book, 4),
    
    logloss_ttt  = round(ll_ttt, 4),
    logloss_book = round(ll_book, 4),
    
    calratio_ttt  = round(cal_ttt, 4),
    calratio_book = round(cal_book, 4),
    
    mov_pred_scale = MOV_PRED_SCALE,
    mov_pred_cap   = MOV_PRED_CAP,
    
    preds_file = preds_path
  )
  write_csv(summary_df, summary_path)
  
  list(
    set_name = set_name,
    ks = ks,
    n_comp = n_comp,
    acc_ttt = 100*acc_ttt,
    acc_book = 100*acc_book,
    brier_ttt = brier_ttt,
    brier_book = brier_book,
    logloss_ttt = ll_ttt,
    logloss_book = ll_book,
    calratio_ttt = cal_ttt,
    calratio_book = cal_book,
    preds_path = preds_path,
    summary_path = summary_path
  )
}

# ----------------------------
# 6) Run all sets + combined summary
# ----------------------------
results <- lapply(names(TOURNAMENT_SETS), function(nm) {
  run_ttt_for_set(nm, TOURNAMENT_SETS[[nm]])
})

results_df <- bind_rows(lapply(results, function(x) {
  tibble(
    tournament_set = x$set_name,
    n_tournaments_used = length(x$ks),
    n_matches = x$n_comp,
    
    acc_ttt = round(x$acc_ttt, 2),
    acc_book = round(x$acc_book, 2),
    
    brier_ttt = round(x$brier_ttt, 4),
    brier_book = round(x$brier_book, 4),
    
    logloss_ttt = round(x$logloss_ttt, 4),
    logloss_book = round(x$logloss_book, 4),
    
    calratio_ttt = round(x$calratio_ttt, 4),
    calratio_book = round(x$calratio_book, 4),
    
    preds_file = x$preds_path,
    summary_file = x$summary_path
  )
})) %>%
  arrange(desc(acc_ttt))

write_csv(results_df, "metrics_summary_ALLSETS_largewins.csv")

print(results_df)
