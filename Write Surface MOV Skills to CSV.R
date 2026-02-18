library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(lubridate)
library(stringr)
library(readr)

# ----------------------------
# 0) FORCE CORRECT TTT SOURCE (your modified refclass History)
# ----------------------------
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
# 1) OPTIMAL PARAMS
# ----------------------------
BEST_MOV_W_SCALE <- 0.35
BEST_MOV_W_CAP   <- 3.0

SIGMA0 <- 8
GAMMA0 <- 0.006

EPS0   <- 0.01
ITERS0 <- 4

# ----------------------------
# 2) Surface tagging helper
# ----------------------------
`%||%` <- function(a,b) if (is.null(a)) b else a

surf_tag <- function(s) {
  s <- toupper(trimws(s %||% ""))
  if (grepl("CLAY", s))  return("CLAY")
  if (grepl("GRASS", s)) return("GRASS")
  "HARD"
}

# ----------------------------
# 3) Load & prep data
# ----------------------------
df0 <- read_excel("2023_2024_ATP_Results.xlsx") %>%
  mutate(
    Date    = as.Date(Date),
    Winner  = trimws(Winner),
    Loser   = trimws(Loser),
    Surface = trimws(Surface),
    SurfTag = vapply(Surface, surf_tag, character(1)),
    Round   = trimws(Round)
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
# 4) Helper: build inputs using total games AND surface nodes
# ----------------------------
build_ttt_inputs_tennis_surface <- function(
    df,
    winner_col  = "Winner",
    loser_col   = "Loser",
    surface_col = "SurfTag",
    date_col    = "Date",
    w_prefix    = "W",
    l_prefix    = "L",
    max_sets    = 5L,
    time_origin = "1970-01-01"
) {
  w_cols <- paste0(w_prefix, seq_len(max_sets))
  l_cols <- paste0(l_prefix, seq_len(max_sets))
  
  missing_cols <- setdiff(c(winner_col, loser_col, surface_col, date_col, w_cols, l_cols), names(df))
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
    stag  <- as.character(df[[surface_col]][i])
    
    wid <- paste0(wname, "_", stag)
    lid <- paste0(lname, "_", stag)
    
    composition[[i]] <- list(c(wid), c(lid))
    results[[i]]     <- c(games_w[i], games_l[i])
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
# 5) Rolling snapshot loop (by ATP k)
# ----------------------------
skills_by_k <- list()

for (k in ks) {
  
  test_df <- df0 %>%
    filter(year(Date) == 2024, ATP == k)
  if (nrow(test_df) == 0) next
  
  inp_tr <- build_ttt_inputs_tennis_surface(train_pool)
  
  # NOTE: History is a refclass => use $new()
  m <- History$new(
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
  
  stats_df <- map_dfr(names(lc), function(id) {
    last <- lc[[id]][[length(lc[[id]])]][[2]]
    tibble(id = id, mu = last@mu, sigma = last@sigma)
  })
  
  all_ids <- unique(c(
    paste0(train_pool$Winner, "_", train_pool$SurfTag),
    paste0(train_pool$Loser,  "_", train_pool$SurfTag),
    paste0(test_df$Winner,    "_", test_df$SurfTag),
    paste0(test_df$Loser,     "_", test_df$SurfTag)
  ))
  
  stats_wide <- tibble(id = all_ids) %>%
    left_join(stats_df, by = "id") %>%
    replace_na(list(mu = 0, sigma = SIGMA0)) %>%
    separate(id, into = c("player","SurfTag"), sep = "_", extra = "merge") %>%
    mutate(
      ATP         = k,
      mov_w_scale = BEST_MOV_W_SCALE,
      mov_w_cap   = BEST_MOV_W_CAP
    ) %>%
    transmute(ATP, player, SurfTag, mu_surface = mu, sigma_surface = sigma, mov_w_scale, mov_w_cap)
  
  skills_by_k[[length(skills_by_k) + 1]] <- stats_wide
  
  train_pool <- bind_rows(train_pool, test_df)
}

skills_by_k_df <- bind_rows(skills_by_k)

# ----------------------------
# 6) Write CSV
# ----------------------------
out_file <- "surface_MOV_ttt_skills_byATP.csv"
write_csv(skills_by_k_df, out_file)

cat("Saved:\n")
cat(" - ", out_file, "\n", sep = "")

