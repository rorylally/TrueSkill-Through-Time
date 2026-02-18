## A tibble: 9 × 9
#gap_days alpha n_matches ttt_acc book_acc inj_rate n_inj ttt_acc_inj book_acc_inj
#<dbl> <dbl>     <int>   <dbl>    <dbl>    <dbl> <int>       <dbl>        <dbl>
#1       14   0.5      1132    63.9     68.4     71.8   813        64.3         70.7
#2       21   0.5      1132    63.7     68.4     59.0   668        66.8         72.3
#3       14   1        1132    63.6     68.4     71.8   813        64.0         70.7
#4       30   0.5      1132    63.5     68.4     44.1   499        68.1         74.1
#5       21   1        1132    63.1     68.4     59.0   668        65.7         72.3
#6       30   1        1132    63.0     68.4     44.1   499        66.9         74.1
#7       30   0        1132    62.5     68.4     44.1   499        65.7         74.1
#8       21   0        1132    62.5     68.4     59.0   668        64.7         72.3
#9       14   0        1132    62.5     68.4     71.8   813        62.4         70.7

#gap_days alpha n_matches ttt_acc book_acc inj_rate n_inj ttt_acc_inj book_acc_inj
#<dbl> <dbl>     <int>   <dbl>    <dbl>    <dbl> <int>       <dbl>        <dbl>
#1       16   0.5      1132    64.0     68.4     64.6   731        66.5         71.3
#2       18   0.5      1132    64.0     68.4     63.3   717        66.2         71.7
#4       12   0.5      1132    63.8     68.4     82.6   935        64.4         70.4
#3       14   0.5      1132    63.9     68.4     71.8   813        64.3         70.7
#5       10   0.5      1132    63.7     68.4     83.8   949        64.4         70.5
#6        5   0.5      1132    62.9     68.4     99.8  1130        62.8         68.4


# ============================
# FAST Injury (absence proxy) grid eval
# - Reads precomputed skills from skills_snapshot_by_ATP.csv
# - Injury proxy = long absence before tournament (days since last match)
# - Mean penalty: mu_adj = mu - ALPHA * sigma * inj
# - Probability-based prediction
# - SMALL grid: 9 runs
# ============================

library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)

MATCH_FILE  <- "2023_2024_ATP_Results.xlsx"
SKILLS_FILE <- "skills_snapshot_by_ATP.csv"

to_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct","POSIXlt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  as.Date(as.character(x))
}

# ----------------------------
# SMALL GRID (9 runs)
# ----------------------------
GAP_DAYS_GRID <- c(5, 10, 12, 14, 16, 18)
ALPHA_GRID    <- c(0.5)
BETA <- 1.0

# ----------------------------
# Load matches
# ----------------------------
df0 <- read_excel(MATCH_FILE) %>%
  mutate(
    Date    = to_date_safe(Date),
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
# Load skills snapshots (auto-detect)
# ----------------------------
skills0_raw <- read_csv(SKILLS_FILE, show_col_types = FALSE)
nm <- names(skills0_raw)

date_col   <- intersect(nm, c("Date","date","anchor_date","AnchorDate","snapshot_date","SnapshotDate"))
date_col   <- if (length(date_col) > 0) date_col[1] else NA_character_

player_col <- intersect(nm, c("player","Player","name","Name"))
player_col <- if (length(player_col) > 0) player_col[1] else stop("No player column found in skills CSV.")

mu_col     <- intersect(nm, c("mu_generic","mu","Mu","muGeneric"))
mu_col     <- if (length(mu_col) > 0) mu_col[1] else stop("No mu_generic column found in skills CSV.")

sig_col    <- intersect(nm, c("sigma_generic","sigma","Sigma","sigmaGeneric"))
sig_col    <- if (length(sig_col) > 0) sig_col[1] else stop("No sigma_generic column found in skills CSV.")

atp_col    <- intersect(nm, c("ATP","atp","tournament","tourney_id"))
atp_col    <- if (length(atp_col) > 0) atp_col[1] else stop("No ATP column found in skills CSV.")

skills0 <- skills0_raw %>%
  transmute(
    ATP           = .data[[atp_col]],
    player        = trimws(as.character(.data[[player_col]])),
    mu_generic    = as.numeric(.data[[mu_col]]),
    sigma_generic = as.numeric(.data[[sig_col]]),
    Date          = if (!is.na(date_col)) to_date_safe(.data[[date_col]]) else as.Date(NA)
  )

get_skills_for_atp <- function(atp_code, anchor_date, players_vec, skills_tbl) {
  has_dates <- any(!is.na(skills_tbl$Date))
  if (has_dates) {
    s <- skills_tbl %>% filter(ATP == atp_code, Date == anchor_date)
    if (nrow(s) == 0) s <- skills_tbl %>% filter(ATP == atp_code)
  } else {
    s <- skills_tbl %>% filter(ATP == atp_code)
  }
  
  tibble(player = players_vec) %>%
    left_join(s %>% select(player, mu_generic, sigma_generic), by = "player") %>%
    mutate(
      mu_generic    = coalesce(mu_generic, 0),
      sigma_generic = coalesce(sigma_generic, 4)
    )
}

# ----------------------------
# Absence-based injury table
# ----------------------------
build_absence_tbl <- function(train_pool, players_needed, anchor_date, GAP_DAYS) {
  
  last_played <- train_pool %>%
    select(Date, Winner, Loser) %>%
    pivot_longer(cols = c(Winner, Loser), names_to = "role", values_to = "player") %>%
    mutate(player = trimws(player)) %>%
    group_by(player) %>%
    summarise(last_match_date = max(Date), .groups = "drop")
  
  tibble(player = players_needed) %>%
    left_join(last_played, by = "player") %>%
    mutate(
      days_since_play = as.numeric(anchor_date - last_match_date),
      inj_flag = case_when(
        is.na(days_since_play) ~ 0L,          # unseen players treated as no-flag
        days_since_play < 0 ~ 0L,
        days_since_play >= GAP_DAYS ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    select(player, inj_flag, days_since_play)
}

# ----------------------------
# Run one combo
# ----------------------------
run_one_combo <- function(GAP_DAYS, ALPHA) {
  
  train_pool <- df0 %>% filter(year(Date) == 2023)
  all_preds  <- list()
  
  for (k in ks) {
    
    test_df <- df0 %>%
      filter(year(Date) == 2024, ATP == k, Round == "1st Round")
    if (nrow(test_df) == 0) next
    
    anchor_date <- min(test_df$Date, na.rm = TRUE)
    players_needed <- unique(c(test_df$Winner, test_df$Loser))
    
    abs_tbl <- build_absence_tbl(
      train_pool     = train_pool,
      players_needed = players_needed,
      anchor_date    = anchor_date,
      GAP_DAYS       = GAP_DAYS
    )
    
    stats_wide <- get_skills_for_atp(
      atp_code    = k,
      anchor_date = anchor_date,
      players_vec = players_needed,
      skills_tbl  = skills0
    )
    
    preds <- test_df %>%
      left_join(stats_wide, by = c("Winner" = "player")) %>%
      left_join(stats_wide, by = c("Loser"  = "player"), suffix = c("_w","_l")) %>%
      left_join(abs_tbl,    by = c("Winner" = "player")) %>%
      left_join(abs_tbl,    by = c("Loser"  = "player"), suffix = c("_w_abs","_l_abs")) %>%
      mutate(
        mu_winner    = mu_generic_w,
        mu_loser     = mu_generic_l,
        sigma_winner = sigma_generic_w,
        sigma_loser  = sigma_generic_l,
        
        inj_w = as.integer(coalesce(inj_flag_w_abs, 0L)),
        inj_l = as.integer(coalesce(inj_flag_l_abs, 0L)),
        
        # mean penalty (accuracy-relevant)
        mu_w_adj = mu_winner - ALPHA * sigma_winner * inj_w,
        mu_l_adj = mu_loser  - ALPHA * sigma_loser  * inj_l,
        
        z     = (mu_w_adj - mu_l_adj) / sqrt(2 * BETA^2 + sigma_winner^2 + sigma_loser^2),
        p_win = pnorm(z),
        
        pred        = if_else(p_win >= 0.5, Winner, Loser),
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
  
  inj_subset <- comp_df %>% filter(inj_w == 1L | inj_l == 1L)
  
  tibble(
    gap_days    = GAP_DAYS,
    alpha       = ALPHA,
    n_matches   = nrow(comp_df),
    ttt_acc     = round(mean(comp_df$ttt_correct)     * 100, 4),
    book_acc    = round(mean(comp_df$bookie_correct) * 100, 4),
    inj_rate    = round(mean((comp_df$inj_w == 1L) | (comp_df$inj_l == 1L)) * 100, 4),
    n_inj       = nrow(inj_subset),
    ttt_acc_inj = if (nrow(inj_subset) > 0) round(mean(inj_subset$ttt_correct)     * 100, 4) else NA_real_,
    book_acc_inj= if (nrow(inj_subset) > 0) round(mean(inj_subset$bookie_correct) * 100, 4) else NA_real_
  )
}

# ----------------------------
# Run grid (9 combos)
# ----------------------------
grid <- expand.grid(
  GAP_DAYS = GAP_DAYS_GRID,
  ALPHA    = ALPHA_GRID
)

res_list <- vector("list", nrow(grid))

for (i in seq_len(nrow(grid))) {
  g <- grid$GAP_DAYS[i]
  a <- grid$ALPHA[i]
  cat(sprintf("Running %d/%d: GAP_DAYS=%d | ALPHA=%.2f\n", i, nrow(grid), g, a))
  res_list[[i]] <- run_one_combo(GAP_DAYS = g, ALPHA = a)
}

res_df <- bind_rows(res_list) %>%
  arrange(desc(ttt_acc), desc(ttt_acc_inj), desc(inj_rate))

cat("\n=== ABSENCE-INJURY GRID RESULTS ===\n")
print(res_df, n = nrow(res_df))

write_csv(res_df, "injury_absence_grid_results_small.csv")
cat("\nSaved: injury_absence_grid_results_small.csv\n")


