################################################################################
# 00_setup.R
# Shared configuration, helpers, and the master loader for the fixture-
# congestion pipeline. Every other script begins with: source("00_setup.R")
#
# Pipeline order:
#   01_data_construction.R   builds master_analysis.csv (+ companion files)
#   02_descriptives.R        Table 1
#   03_main_tables.R         Tables 2-6
#   04_appendix.R            Tables A1-A5 (A6 stubbed)
#   99_exploratory.R         not-in-paper analyses
################################################################################

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(fixest)
  library(MASS)
  library(conflicted)
})
conflict_prefer("select", "dplyr", quiet = TRUE)
conflict_prefer("filter", "dplyr", quiet = TRUE)
conflict_prefer("lag",    "dplyr", quiet = TRUE)

# ---- Paths -----------------------------------------------------------------
# All paths are relative to the repository root. Open congestion.Rproj in
# RStudio (which sets the working directory to the root automatically), or
# setwd() to the repo root before sourcing. To run from elsewhere, set the
# environment variable CONGESTION_ROOT to the repo path.
root_override <- Sys.getenv("CONGESTION_ROOT", unset = "")
if (nzchar(root_override) && dir.exists(root_override)) setwd(root_override)

raw_path     <- "data/raw/"      # scraped source data -- NOT distributed (see README)
private_path <- "data/private/"  # derived files at source granularity -- NOT distributed
derived_path <- "data/derived/"  # aggregated team-season panel -- INCLUDED in the repo
out_path     <- "output/"        # regression tables and figures
for (p in c(private_path, derived_path, out_path))
  dir.create(p, showWarnings = FALSE, recursive = TRUE)

# Which data tiers are present? 02-04 need only the derived tier.
RAW_DATA_AVAILABLE <- all(file.exists(paste0(raw_path,
  c("final_team_mapping2.csv", "cleaned_injury_data_v2.csv",
    "match_congestion.csv", "teamperformance.xlsx"))))
PRIVATE_DATA_AVAILABLE <- all(file.exists(paste0(private_path,
  c("player_season_raw.csv", "match_level_fifpro.csv"))))

# ---- Parameters used by the position-based appendix cuts (A6) ---------------
POSITION_COL <- "position"          # name of the position column in the injury file
AM_TO_FWD    <- FALSE               # TRUE = group attacking midfielders with forwards
SLOTS        <- c(DEF = 4, MID = 3, FWD = 3)   # canonical 4-3-3 outfield best-k ceiling
N_OUTFIELD_CEILING <- sum(SLOTS)    # = 10, GK-excluded ceiling size

# ============================================================================ #
# HELPERS                                                                      #
# ============================================================================ #

# Gini coefficient of a non-negative vector (squad market-value inequality).
helper_gini <- function(x) {
  x <- x[!is.na(x) & x >= 0]
  if (length(x) < 2 || sum(x) == 0) return(NA_real_)
  x <- sort(x)
  n <- length(x)
  (2 * sum(seq_len(n) * x) / (n * sum(x))) - (n + 1) / n
}

# Mean of the top-k values (the "best-k" market-value ceiling).
topk_mean <- function(v, k) {
  v <- sort(v[!is.na(v)], decreasing = TRUE)
  if (length(v) == 0) return(NA_real_)
  mean(v[seq_len(min(k, length(v)))])
}

# Longest contiguous run of calendar days in a congested 7-day-lookback window,
# expressed in week-equivalents. Operationalizes the FIFPRO Additional Finding
# ("no more than three consecutive weeks with two appearances per week").
# A calendar day d is "congested" if >= 2 matches fall in [d - 7, d].
compute_max_consec_weeks <- function(match_dates) {
  match_dates <- sort(unique(match_dates[!is.na(match_dates)]))
  if (length(match_dates) < 2) return(0L)
  all_days <- seq.Date(min(match_dates), max(match_dates), by = "day")
  counts <- vapply(all_days, function(d) {
    sum(match_dates >= (d - 7) & match_dates <= d)
  }, integer(1))
  is_cong <- counts >= 2
  if (!any(is_cong)) return(0L)
  rl <- rle(is_cong)
  longest_run_days <- max(rl$lengths[rl$values], na.rm = TRUE)
  as.integer(round(longest_run_days / 7))
}

# Map raw position strings to {GK, DEF, MID, FWD}. Verify the printed mapping
# (see 04_appendix.R) covers every value in your data with no NA.
classify_position <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- dplyr::case_when(
    str_detect(x, "goalkeeper|keeper|\\bgk\\b")                          ~ "GK",
    str_detect(x, "back|defender|defence|defense|libero|\\bcb\\b|\\blb\\b|\\brb\\b|\\bwb\\b") ~ "DEF",
    str_detect(x, "midfield|midfielder|\\bdm\\b|\\bcm\\b|\\bam\\b")       ~ "MID",
    str_detect(x, "forward|striker|wing|\\bcf\\b|\\bst\\b")              ~ "FWD",
    TRUE                                                                 ~ NA_character_
  )
  if (AM_TO_FWD) out[str_detect(x, "attack") & out == "MID"] <- "FWD"
  out
}

# Read the master file and (re)derive the factor identifiers fixest uses for FE.
load_master <- function() {
  m <- readr::read_csv(file.path(derived_path, "master_analysis.csv"),
                       show_col_types = FALSE)
  m %>%
    mutate(
      team_id          = as.factor(Team),
      season_id        = as.factor(Season),
      league_id        = as.factor(League),
      league_season_id = as.factor(paste0(League, "_", Season))
    )
}

