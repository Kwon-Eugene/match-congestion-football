################################################################################
# 01_data_construction.R
#
# Harmonizes the three raw sources, builds the team-season master with every
# derived variable the analysis needs, and writes the companion files used
# downstream. Run this once; 02/03/04 then read the outputs.
#
# INPUTS (data/):
#   final_team_mapping2.csv     team-name harmonization across sources
#   cleaned_injury_data_v2.csv  player-injury records (Transfermarkt)
#   match_congestion.csv        match-level fixtures (FBref)
#   teamperformance.xlsx        team-season performance (FBref)
#
# OUTPUTS (output/):
#   master_analysis.csv         team-season master, n=976 (workhorse file)
#   player_season_raw.csv       player-season Min/MV/position (feeds A6)
#   match_level_fifpro.csv      match-level with FIFPRO flags (feeds A5, 99)
#   cb_league_season_panel.csv  league-season Noll-Scully panel (feeds 99)
#
# FIXES APPLIED vs the old monolithic script:
#   - squad_total_value renamed to total_squad_value (one consistent name).
#   - high_mv_inequality, deployment_intensity, log_total_matches, and the
#     lagged outcome are built HERE and stored in master, so no script needs
#     the undefined `master_fifpro` object.
################################################################################

source("00_setup.R")

if (!RAW_DATA_AVAILABLE)
  stop("Raw source data not found in data/raw/. These files are scraped from\n",
       "  Transfermarkt and FBref and are NOT distributed with this repository\n",
       "  (see README: Data availability). All paper tables can be reproduced\n",
       "  WITHOUT this script from data/derived/master_analysis.csv via 02-04.")

# ============================================================================ #
# 1. READ + LOOKUPS                                                            #
# ============================================================================ #

mapping         <- read_csv(paste0(raw_path, "final_team_mapping2.csv"),
                            show_col_types = FALSE)
injury_raw      <- read_csv(paste0(raw_path, "cleaned_injury_data_v2.csv"),
                            show_col_types = FALSE)
congestion_raw  <- read_csv(paste0(raw_path, "match_congestion.csv"),
                            show_col_types = FALSE)
performance_raw <- read_excel(paste0(raw_path, "teamperformance.xlsx"))

season_lookup <- tibble(
  season_start_year = 2015:2024,
  Season            = c(1516, 1617, 1718, 1819, 1920, 2021,
                        2122, 2223, 2324, 2425)
)

# comp_name in the injury data reflects each squad's CURRENT competition, not
# the historical one, so we use it only as a COUNTRY indicator. The actual
# competition is fixed by the FBref performance data (top flight); any
# team-season not in master is dropped at the merge.
league_lookup <- tibble(
  comp_name_injury = c(
    "Premier League", "Bundesliga", "Serie A", "Ligue 1", "LaLiga",
    "2. Bundesliga", "Championship", "Serie B", "Ligue 2", "LaLiga2",
    "3. Liga", "League One", "Serie C - Girone B", "Serie C - Girone C",
    "Serie D - Girone B", "Championnat National",
    "Championnat National 2 - Groupe A", "Championnat National 2 - Groupe C",
    "Championnat National 3 - Groupe E"),
  League = c("ENG", "GER", "ITA", "FRA", "ESP",
             "GER", "ENG", "ITA", "FRA", "ESP",
             "GER", "ENG", "ITA", "ITA",
             "ITA", "FRA",
             "FRA", "FRA",
             "FRA")
)

# ============================================================================ #
# 2. HARMONIZATION                                                             #
# ============================================================================ #

# Do NOT filter on !is.na(League) here; the merge into master restricts to
# top-flight seasons. Filtering would wrongly drop team-seasons whose squad's
# comp_name reflects a later (post-relegation) competition.
injury <- injury_raw %>%
  left_join(mapping, by = c("squad" = "Injury_Squad_Name")) %>%
  rename(Team = Performance_Name) %>%
  left_join(season_lookup, by = "season_start_year") %>%
  left_join(league_lookup, by = c("comp_name" = "comp_name_injury")) %>%
  select(-Congestion_Team_Name) %>%
  filter(!is.na(Team), !is.na(Season))

# Rename happens exactly once; downstream uses the renamed `Team` and never
# rejoins mapping.
congestion <- congestion_raw %>%
  left_join(mapping, by = c("Team" = "Congestion_Team_Name")) %>%
  rename(Team_original = Team, Team = Performance_Name) %>%
  select(-Injury_Squad_Name)

# ============================================================================ #
# 3. TEAM-SEASON AGGREGATES                                                    #
# ============================================================================ #

# --- Injuries: counts and value-weighted cost by category -------------------
# `cost` in the raw file = player_market_value_euro * games_missed_count.
injury_agg <- injury %>%
  group_by(League, Team, Season) %>%
  summarise(
    total_injuries     = sum(!is.na(injury)),
    fatigue_injuries   = sum(injury_category == "Fatigue/Soft Tissue" & !is.na(injury), na.rm = TRUE),
    trauma_injuries    = sum(injury_category == "Trauma/Impact"       & !is.na(injury), na.rm = TRUE),
    illness_injuries   = sum(injury_category == "Illness"             & !is.na(injury), na.rm = TRUE),
    other_injuries     = sum(injury_category == "Other/Surgical"      & !is.na(injury), na.rm = TRUE),
    total_days_missed  = sum(days_missed, na.rm = TRUE),
    total_games_missed = sum(games_missed_count, na.rm = TRUE),
    total_injury_cost  = sum(cost, na.rm = TRUE),
    fatigue_cost       = sum(ifelse(injury_category == "Fatigue/Soft Tissue", cost, 0), na.rm = TRUE),
    trauma_cost        = sum(ifelse(injury_category == "Trauma/Impact",       cost, 0), na.rm = TRUE),
    illness_cost       = sum(ifelse(injury_category == "Illness",             cost, 0), na.rm = TRUE),
    other_cost         = sum(ifelse(injury_category == "Other/Surgical",      cost, 0), na.rm = TRUE),
    .groups = "drop"
  )

# --- Player-season table (deduplicated; carries position if available) ------
pos_in_data <- POSITION_COL %in% names(injury)
if (!pos_in_data)
  message("NOTE: position column '", POSITION_COL,
          "' not found; player_season_raw.pos_raw will be NA and A6 will warn.")

player_season <- injury %>%
  filter(!is.na(Player), !is.na(Min)) %>%
  group_by(League, Team, Season, Player) %>%
  summarise(
    Min          = first(Min),
    market_value = first(player_market_value_euro),
    pos_raw      = if (pos_in_data) first(.data[[POSITION_COL]]) else NA_character_,
    .groups      = "drop"
  )

write_csv(player_season, paste0(private_path, "player_season_raw.csv"))

# --- Value / deployment aggregates per team-season --------------------------
deployment_agg <- player_season %>%
  group_by(League, Team, Season) %>%
  mutate(
    mv_quartile = ntile(market_value, 4),
    mv_rank     = rank(-market_value, ties.method = "first")
  ) %>%
  summarise(
    squad_total_minutes    = sum(Min, na.rm = TRUE),
    total_squad_value      = sum(market_value, na.rm = TRUE),   # was squad_total_value
    squad_mean_value       = mean(market_value, na.rm = TRUE),
    squad_median_value     = median(market_value, na.rm = TRUE),
    top_q_minutes_share    = sum(Min[mv_quartile == 4], na.rm = TRUE) / pmax(sum(Min, na.rm = TRUE), 1),
    bottom_q_minutes_share = sum(Min[mv_quartile == 1], na.rm = TRUE) / pmax(sum(Min, na.rm = TRUE), 1),
    top3_minutes_share     = sum(Min[mv_rank <= 3], na.rm = TRUE)     / pmax(sum(Min, na.rm = TRUE), 1),
    mv_gini                = helper_gini(market_value),
    mv_cr3                 = sum(market_value[mv_rank <= 3], na.rm = TRUE) / pmax(sum(market_value, na.rm = TRUE), 1),
    avg_mv_per_minute      = sum(market_value * Min, na.rm = TRUE)    / pmax(sum(Min, na.rm = TRUE), 1),
    n_players_with_mv      = sum(!is.na(market_value)),
    .groups = "drop"
  )

# Maximum-talent benchmark: mean MV of the 11 most valuable squad members.
max_possible_mv <- player_season %>%
  filter(!is.na(market_value)) %>%
  group_by(League, Team, Season) %>%
  summarise(max_possible_avg_mv = topk_mean(market_value, 11), .groups = "drop")

# --- Congestion aggregates per team-season ----------------------------------
congestion_agg <- congestion %>%
  filter(!is.na(Team)) %>%
  mutate(GF = as.numeric(GF), GA = as.numeric(GA)) %>%
  group_by(League, Team, Season) %>%
  summarise(
    total_matches      = n(),
    domestic_matches   = sum(Comp %in% c("Serie A", "Premier League", "La Liga", "Ligue 1", "Bundesliga")),
    european_matches   = sum(Comp %in% c("Champions Lg", "Europa Lg", "Conf Lg")),
    conf_lg_matches    = sum(Comp == "Conf Lg"),
    cup_matches        = total_matches - domestic_matches - european_matches,
    mean_recovery_days = mean(Recovery_Hours, na.rm = TRUE),   # mislabeled upstream; in days
    mean_matches_L7D   = mean(Matches_L7D, na.rm = TRUE),
    mean_matches_L14D  = mean(Matches_L14D, na.rm = TRUE),
    mean_matches_L21D  = mean(Matches_L21D, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(european_team = as.integer(european_matches > 0))

# --- Performance prep -------------------------------------------------------
performance <- performance_raw %>%
  mutate(
    Total_Pts      = League_Home_Pts + League_Away_Pts,
    post_5subs     = as.integer(Season >= 2021),
    post_conf_lg   = as.integer(Season >= 2122),
    covid_season   = as.integer(Season %in% c(1920, 2021)),
    ligue1_18teams = as.integer(League == "FRA" & Season >= 2324)
  )

# ============================================================================ #
# 4. MASTER FILE                                                               #
# ============================================================================ #

master <- performance %>%
  left_join(congestion_agg, by = c("League", "Team", "Season")) %>%
  left_join(injury_agg,     by = c("League", "Team", "Season")) %>%
  left_join(deployment_agg, by = c("League", "Team", "Season")) %>%
  left_join(max_possible_mv, by = c("League", "Team", "Season")) %>%
  mutate(
    Pts_per_Match    = League_PointsperMatch,
    log_injury_cost  = log(total_injury_cost + 1),
    log_fatigue      = log(fatigue_injuries + 1),
    log_trauma       = log(trauma_injuries + 1),
    log_fatigue_cost = log(fatigue_cost + 1),
    log_trauma_cost  = log(trauma_cost + 1)
  )

# ---- FIFPRO-aligned exposure measures --------------------------------------
match_full <- congestion %>%
  filter(!is.na(Team)) %>%
  mutate(
    Date           = as.Date(Date),
    Recovery_Days  = Recovery_Hours,
    fifpro_v2d     = as.integer(Recovery_Days < 2),
    fifpro_v3d     = as.integer(Recovery_Days < 3),
    fifpro_v35d    = as.integer(Recovery_Days < 3.5),
    fifpro_v4d     = as.integer(Recovery_Days < 4),
    congested_week = as.integer(Matches_L7D >= 2)
  )

write_csv(match_full, paste0(private_path, "match_level_fifpro.csv"))

fifpro_ts <- match_full %>%
  group_by(League, Team, Season) %>%
  summarise(
    n_matches_with_recovery = sum(!is.na(Recovery_Days)),
    n_fifpro_v2d      = sum(fifpro_v2d, na.rm = TRUE),
    n_fifpro_v3d      = sum(fifpro_v3d, na.rm = TRUE),
    n_fifpro_v35d     = sum(fifpro_v35d, na.rm = TRUE),
    n_fifpro_v4d      = sum(fifpro_v4d, na.rm = TRUE),
    n_congested_weeks = sum(congested_week, na.rm = TRUE),
    .groups = "drop"
  )

max_consec <- match_full %>%
  filter(!is.na(Date)) %>%
  group_by(League, Team, Season) %>%
  summarise(max_consec_congested_weeks = compute_max_consec_weeks(Date), .groups = "drop")

fifpro_ts <- fifpro_ts %>%
  left_join(max_consec, by = c("League", "Team", "Season")) %>%
  mutate(
    max_consec_congested_weeks = replace_na(max_consec_congested_weeks, 0L),
    fifpro_addl_violation      = as.integer(max_consec_congested_weeks >= 3)
  )

master <- master %>%
  left_join(fifpro_ts, by = c("League", "Team", "Season"))

# ---- Analysis-ready derived variables (built once, stored in master) -------
master <- master %>%
  arrange(Team, Season) %>%
  group_by(Team) %>%
  mutate(Pts_per_Match_lag = lag(Pts_per_Match, 1)) %>%
  ungroup() %>%
  mutate(
    log_total_matches    = log(total_matches + 1),
    high_mv_inequality   = as.integer(mv_gini > median(mv_gini, na.rm = TRUE)),
    deployment_intensity = avg_mv_per_minute / pmax(max_possible_avg_mv, 1)
  )

cat("\n=== MASTER FILE ===\n")
cat("Rows:", nrow(master), "| Cols:", ncol(master), "\n")
cat("Teams:", n_distinct(master$Team), "| Seasons:", n_distinct(master$Season),
    "| Leagues:", n_distinct(master$League), "\n")
cat("Team-seasons with max_consec defined:",
    sum(!is.na(master$max_consec_congested_weeks)), "\n")
cat("Mean total_squad_value (M EUR):",
    round(mean(master$total_squad_value, na.rm = TRUE) / 1e6, 1),
    "  <-- sanity-check against your previous total_squad_value\n")

write_csv(master, paste0(derived_path, "master_analysis.csv"))

# ============================================================================ #
# 5. LEAGUE-SEASON COMPETITIVE-BALANCE PANEL (descriptive; feeds 99)           #
# ============================================================================ #
# n = 50 (5 leagues x 10 seasons); underpowered for inference. The inferential
# CB result is at the team-season level in 03 (Table 5).
cb_panel <- master %>%
  group_by(League, Season) %>%
  summarise(
    n_teams             = n(),
    matches_per_team    = first(League_Match.Played),
    mean_pts            = mean(Total_Pts),
    sd_pts              = sd(Total_Pts),
    sd_ideal_precise    = sqrt(14 * first(League_Match.Played)) / 3,
    noll_scully_precise = sd(Total_Pts) / (sqrt(14 * first(League_Match.Played)) / 3),
    hhi_normalized      = (sum((Total_Pts / sum(Total_Pts))^2) - 1/n()) / (1 - 1/n()),
    cr4                 = sum(sort(Total_Pts, decreasing = TRUE)[1:4]) / sum(Total_Pts),
    pts_range           = max(Total_Pts) - min(Total_Pts),
    post_5subs          = first(post_5subs),
    post_conf_lg        = first(post_conf_lg),
    covid_season        = first(covid_season),
    ligue1_18teams      = first(ligue1_18teams),
    .groups = "drop"
  )
write_csv(cb_panel, paste0(derived_path, "cb_league_season_panel.csv"))

cat("\n=== 01 COMPLETE: derived files in ", derived_path, " | private files in ", private_path, " ===\n", sep = "")

