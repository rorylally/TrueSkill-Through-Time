# ============================================================
# SKILLS SNAPSHOT BY ATP TOURNAMENT (BEST PARAMS, NO GRID)
# ============================================================

library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(readr)
if ("package:TrueSkillThroughTime" %in% search()) {
  detach("package:TrueSkillThroughTime", unload = TRUE, character.only = TRUE)
}
TTT_SRC <- "ttt_src/TrueSkillThroughTimeGameAggregate2/R/trueskillthroughtime.R"
source(TTT_SRC)

# sanity check: History is a refclass with initialize args we need
# sanity check: History is a refclass with initialize args we need
if (!inherits(History, "refObjectGenerator")) {
  stop("History is not a refclass generator (refObjectGenerator). Wrong code loaded.")
}

init_fun  <- History$def@refMethods$initialize
init_args <- names(formals(init_fun))

need_args <- c("results","times","use_mov_weight","mov_w_scale","mov_w_cap")
miss <- setdiff(need_args, init_args)
if (length(miss) > 0) stop("Wrong History loaded; missing: ", paste(miss, collapse=", "))
message("✅ Using modified History refclass from: ", TTT_SRC)



# ----------------------------
# 0) SET YOUR OPTIMAL PARAMS HERE
# ----------------------------
BEST_MOV_W_SCALE <- 0.35
BEST_MOV_W_CAP   <- 3.0

# Model hyperparams (keep consistent with your baseline)
SIGMA0 <- 4
GAMMA0 <- 0.006

# Convergence controls
EPS0   <- 0.01
ITERS0 <- 4

# ----------------------------
# 1) Load & prep data
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    Court   = trimws(Court)
  )

# ---- Seed training with 2023 ----
train_pool <- df0 %>% filter(year(Date) == 2023)

# ---- 2024 tournament codes ----
ks <- df0 %>%
  filter(year(Date) == 2024) %>%
  distinct(ATP) %>%
  arrange(ATP) %>%
  pull(ATP)

# ----------------------------
# 2) Helper: build inputs using total games (W1..W5 / L1..L5)
# ----------------------------
build_ttt_inputs_tennis <- function(
    df,
    winner_col = "Winner",
    loser_col  = "Loser",
    date_col   = "Date",
    w_prefix   = "W",
    l_prefix   = "L",
    max_sets   = 5L,
    time_origin = "1970-01-01"
) {
  w_cols <- paste0(w_prefix, seq_len(max_sets))
  l_cols <- paste0(l_prefix, seq_len(max_sets))
  
  missing_cols <- setdiff(c(winner_col, loser_col, date_col, w_cols, l_cols), names(df))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  for (cc in c(w_cols, l_cols)) df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
  
  games_w <- rowSums(df[w_cols], na.rm = TRUE)
  games_l <- rowSums(df[l_cols], na.rm = TRUE)
  
  valid_score <- (games_w + games_l) > 0 & (games_w != games_l)
  
  composition <- vector("list", nrow(df))
  results     <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    if (!valid_score[i]) {
      composition[[i]] <- NULL
      results[[i]] <- NULL
      next
    }
    
    wname <- as.character(df[[winner_col]][i])
    lname <- as.character(df[[loser_col]][i])
    
    composition[[i]] <- list(
      c(paste0(wname, "_generic")),
      c(paste0(lname, "_generic"))
    )
    results[[i]] <- c(games_w[i], games_l[i])
  }
  
  d <- as.Date(df[[date_col]])
  times <- as.numeric(d - as.Date(time_origin))
  
  keep <- which(valid_score)
  list(
    composition = composition[keep],
    results     = results[keep],
    times       = times[keep]
  )
}

# ----------------------------
# 3) Rolling snapshot loop
# ----------------------------
skills_by_k <- list()

for (k in ks) {
  
  test_df <- df0 %>%
    filter(year(Date) == 2024, ATP == k)
  if (nrow(test_df) == 0) next
  
  inp_tr <- build_ttt_inputs_tennis(train_pool)
  
  m <- History(
    composition    = inp_tr$composition,
    results        = inp_tr$results,
    times          = inp_tr$times,
    sigma          = SIGMA0,
    gamma          = GAMMA0,
    use_mov_weight = TRUE,
    mov_w_scale    = BEST_MOV_W_SCALE,
    mov_w_cap      = BEST_MOV_W_CAP
  )
  
  m$convergence(epsilon = EPS0, iterations = ITERS0)
  
  lc <- m$learning_curves()
  
  stats_df <- map_dfr(names(lc), function(dim) {
    last <- lc[[dim]][[length(lc[[dim]])]][[2]]
    tibble(player_dim = dim, mu = last@mu, sigma = last@sigma)
  }) %>%
    separate(player_dim, into = c("player","skill"), sep = "_", extra = "merge") %>%
    filter(skill == "generic") %>%
    transmute(player, mu_generic = mu, sigma_generic = sigma)
  
  all_players <- unique(c(train_pool$Winner, train_pool$Loser,
                          test_df$Winner,  test_df$Loser))
  
  stats_wide <- tibble(player = all_players) %>%
    left_join(stats_df, by = "player") %>%
    replace_na(list(mu_generic = 0, sigma_generic = SIGMA0)) %>%
    mutate(
      ATP = k,
      mov_w_scale = BEST_MOV_W_SCALE,
      mov_w_cap   = BEST_MOV_W_CAP
    )
  
  skills_by_k[[length(skills_by_k) + 1]] <- stats_wide
  
  # expanding pool
  train_pool <- bind_rows(train_pool, test_df)
}

skills_by_k_df <- bind_rows(skills_by_k)

# ----------------------------
# 4) Write CSV
# ----------------------------
out_file <- sprintf(
  "skills_snapshot_by_ATP_best_MOVv2.csv",
  BEST_MOV_W_SCALE, BEST_MOV_W_CAP
)

write_csv(skills_by_k_df, out_file)

cat("Saved:\n")
cat(" - ", out_file, "\n", sep = "")
