################################################################################
# 04_appendix.R
#
# Appendix tables. Reads ONLY data/derived/master_analysis.csv, so every
# table here is reproducible from the distributed derived data.
#
# OUTPUTS:
#   tableA1_altfifpro.txt              rhythm-vs-count across FIFPRO measures
#   tableA2_control_buildup.txt        none -> +talent -> +talent+exposure
#   tableA3_selection.txt              lagged-outcome control
#   tableA4_deployment_shares.txt      top-quartile and top-3 minutes shares
#   tableA5_did.txt                    Conference-League DiD + event study
#   tableA6a_deployment_gk_excluded.txt
#   tableA6b_deployment_by_position.txt
################################################################################

source("00_setup.R")
master     <- load_master()

mv  <- master %>% filter(!is.na(max_consec_congested_weeks), !is.na(total_squad_value))
mvg <- mv %>% filter(!is.na(mv_gini))

# ============================================================================ #
# TABLE A1: Alternative FIFPRO measures (rhythm vs count)                       #
# ============================================================================ #
# Same triplet across n_fifpro_v3d, n_fifpro_v35d, and the binary Additional-
# Finding violation; with and without the log(total_matches) exposure control.
# ============================================================================ #

fifpro_triplet <- function(measure, exposure = TRUE) {
  exp_term <- if (exposure) " + log_total_matches" else ""
  ff_fat <- as.formula(paste0("fatigue_injuries ~ ", measure, exp_term,
              " + Num.Players + Av.Age + post_5subs + covid_season | team_id + season_id"))
  ff_tra <- as.formula(paste0("trauma_injuries ~ ", measure, exp_term,
              " + Num.Players + Av.Age + post_5subs + covid_season | team_id + season_id"))
  ff_pts <- as.formula(paste0("Pts_per_Match ~ ", measure, exp_term,
              " + Num.Players + Av.Age | team_id + league_season_id"))
  list(fat = fepois(ff_fat, data = mv, vcov = ~team_id),
       tra = fepois(ff_tra, data = mv, vcov = ~team_id),
       pts = feols(ff_pts, data = mv, vcov = ~team_id))
}

measures <- c("n_fifpro_v3d", "n_fifpro_v35d", "fifpro_addl_violation")
no_exp   <- lapply(measures, fifpro_triplet, exposure = FALSE)
with_exp <- lapply(measures, fifpro_triplet, exposure = TRUE)

sink(paste0(out_path, "tableA1_altfifpro.txt"))
cat("TABLE A1: Alternative FIFPRO measures (rhythm vs count)\n\n")
for (i in seq_along(measures)) {
  cat("--- ", measures[i], " : no exposure control ---\n", sep = "")
  print(etable(no_exp[[i]]$fat, no_exp[[i]]$tra, no_exp[[i]]$pts,
               headers = c("Fatigue", "Trauma (placebo)", "Pts/Match"),
               se.below = TRUE, fitstat = ~ n + r2))
  cat("\n--- ", measures[i], " : with log(total_matches) ---\n", sep = "")
  print(etable(with_exp[[i]]$fat, with_exp[[i]]$tra, with_exp[[i]]$pts,
               headers = c("Fatigue", "Trauma (placebo)", "Pts/Match"),
               se.below = TRUE, fitstat = ~ n + r2))
  cat("\n")
}
sink()
cat("\n=== A1 written ===\n")

# ============================================================================ #
# TABLE A2: Control build-up (identification of the points null)               #
# ============================================================================ #
# none -> + talent -> + talent + exposure, for each outcome. Shows the points
# coefficient is positive uncontrolled and attenuates to null with controls;
# fatigue is stable; trauma stays null (placebo).
# ============================================================================ #

buildup <- function(extra) {
  ff_fat <- as.formula(paste0("fatigue_injuries ~ max_consec_congested_weeks", extra,
              " + Num.Players + Av.Age + post_5subs + covid_season | team_id + season_id"))
  ff_tra <- as.formula(paste0("trauma_injuries ~ max_consec_congested_weeks", extra,
              " + Num.Players + Av.Age + post_5subs + covid_season | team_id + season_id"))
  ff_pts <- as.formula(paste0("Pts_per_Match ~ max_consec_congested_weeks", extra,
              " + Num.Players + Av.Age | team_id + league_season_id"))
  list(fat = fepois(ff_fat, data = mv, vcov = ~team_id),
       tra = fepois(ff_tra, data = mv, vcov = ~team_id),
       pts = feols(ff_pts, data = mv, vcov = ~team_id))
}

b_none <- buildup("")
b_tal  <- buildup(" + log(total_squad_value + 1)")
b_te   <- buildup(" + log(total_squad_value + 1) + log_total_matches")

sink(paste0(out_path, "tableA2_control_buildup.txt"))
cat("TABLE A2: Control build-up (none -> +talent -> +talent+exposure)\n\n")
cat("--- Fatigue (Poisson) ---\n")
print(etable(b_none$fat, b_tal$fat, b_te$fat,
             headers = c("None", "+ Talent", "+ Talent + Exposure"),
             se.below = TRUE, fitstat = ~ n))
cat("\n--- Trauma (Poisson, placebo) ---\n")
print(etable(b_none$tra, b_tal$tra, b_te$tra,
             headers = c("None", "+ Talent", "+ Talent + Exposure"),
             se.below = TRUE, fitstat = ~ n))
cat("\n--- Points/Match (OLS) ---\n")
print(etable(b_none$pts, b_tal$pts, b_te$pts,
             headers = c("None", "+ Talent", "+ Talent + Exposure"),
             se.below = TRUE, fitstat = ~ n + r2))
sink()
cat("=== A2 written ===\n")

# ============================================================================ #
# TABLE A3: Selection vs causation (lagged-outcome control)                     #
# ============================================================================ #
# Same estimand and controls as the main spec (rhythm effect net of volume +
# talent), adding last season's points to absorb persistent team quality. If
# the congestion-points link were selection on ability, the lagged control would
# attenuate it; the points null is unchanged, so it is not that. (A switcher-
# subsample lens was dropped: within teams that move in and out of Europe,
# rhythm and volume are collinear, within-team corr ~0.70, so that subsample
# cannot separate the two.)
# ============================================================================ #

m_sel_consec     <- feols(Pts_per_Match ~ max_consec_congested_weeks +
                            log(total_squad_value + 1) + log_total_matches +
                            Num.Players + Av.Age | team_id + league_season_id,
                          data = mv, vcov = ~team_id)
m_sel_consec_lag <- feols(Pts_per_Match ~ max_consec_congested_weeks +
                            Pts_per_Match_lag + log(total_squad_value + 1) +
                            log_total_matches + Num.Players + Av.Age |
                            team_id + league_season_id,
                          data = mv %>% filter(!is.na(Pts_per_Match_lag)),
                          vcov = ~team_id)

sink(paste0(out_path, "tableA3_selection.txt"))
cat("TABLE A3: Selection vs causation (lagged-outcome control)\n")
cat("(both models carry log(total_squad_value+1) + log_total_matches, matching Table 2)\n\n")
print(etable(m_sel_consec, m_sel_consec_lag,
             headers = c("Consec (no lag)", "Consec (+ lag)"),
             se.below = TRUE, fitstat = ~ n + r2))
sink()
cat("=== A3 written ===\n")

# ============================================================================ #
# TABLE A4: Deployment invariance (minutes-share measures)                     #
# ============================================================================ #
# Companion to Table 4 using raw minutes shares rather than the value ratio.
# ============================================================================ #

dshare <- mv %>% filter(!is.na(top_q_minutes_share), !is.na(top3_minutes_share))

m_topq      <- feols(top_q_minutes_share ~ max_consec_congested_weeks +
                       Num.Players + Av.Age | team_id + season_id,
                     data = dshare, vcov = ~team_id)
m_topq_exp  <- feols(top_q_minutes_share ~ max_consec_congested_weeks + log_total_matches +
                       Num.Players + Av.Age | team_id + season_id,
                     data = dshare, vcov = ~team_id)
m_top3      <- feols(top3_minutes_share ~ max_consec_congested_weeks +
                       Num.Players + Av.Age | team_id + season_id,
                     data = dshare, vcov = ~team_id)
m_top3_exp  <- feols(top3_minutes_share ~ max_consec_congested_weeks + log_total_matches +
                       Num.Players + Av.Age | team_id + season_id,
                     data = dshare, vcov = ~team_id)

sink(paste0(out_path, "tableA4_deployment_shares.txt"))
cat("TABLE A4: Deployment invariance (minutes shares)\n\n")
print(etable(m_topq, m_topq_exp, m_top3, m_top3_exp,
             headers = c("Top-Q", "Top-Q + exp", "Top-3", "Top-3 + exp"),
             se.below = TRUE, fitstat = ~ n + r2))
sink()
cat("=== A4 written ===\n")

# ============================================================================ #
# TABLE A5: Conference-League difference-in-differences                        #
# ============================================================================ #
# 2021/22 ECL launch as a quasi-experiment. Pre-trends flat; post small and
# insignificant -- consistent with but underpowered to confirm the cross-section.
# ============================================================================ #

ecl_teams <- master %>% filter(conf_lg_matches > 0) %>% pull(Team) %>% unique()
near_miss <- master %>%
  group_by(Team) %>%
  summarise(median_rank = median(League_Rank, na.rm = TRUE), .groups = "drop") %>%
  filter(median_rank >= 5, median_rank <= 12) %>% pull(Team)

did_data <- master %>%
  mutate(treated_team = as.integer(Team %in% ecl_teams),
         post_period  = as.integer(Season >= 2122),
         treat_post   = treated_team * post_period,
         rel_season   = case_when(
           Season == 1516 ~ -6L, Season == 1617 ~ -5L, Season == 1718 ~ -4L,
           Season == 1819 ~ -3L, Season == 1920 ~ -2L, Season == 2021 ~ -1L,
           Season == 2122 ~  0L, Season == 2223 ~  1L, Season == 2324 ~  2L,
           Season == 2425 ~  3L)) %>%
  filter(!is.na(fatigue_injuries))
did_nm <- did_data %>% filter(Team %in% near_miss)

m_did_fat_full <- fepois(fatigue_injuries ~ treat_post + Num.Players + Av.Age +
                           post_5subs + covid_season | team_id + season_id,
                         data = did_data, vcov = ~team_id)
m_did_fat_nm   <- fepois(fatigue_injuries ~ treat_post + Num.Players + Av.Age +
                           post_5subs + covid_season | team_id + season_id,
                         data = did_nm, vcov = ~team_id)
m_did_tra_full <- fepois(trauma_injuries ~ treat_post + Num.Players + Av.Age +
                           post_5subs + covid_season | team_id + season_id,
                         data = did_data, vcov = ~team_id)
m_event_fat <- feols(log_fatigue ~ i(rel_season, treated_team, ref = -1) +
                       Num.Players + Av.Age | team_id + season_id,
                     data = did_data, vcov = ~team_id)
m_event_tra <- feols(log_trauma ~ i(rel_season, treated_team, ref = -1) +
                       Num.Players + Av.Age | team_id + season_id,
                     data = did_data, vcov = ~team_id)

sink(paste0(out_path, "tableA5_did.txt"))
cat("TABLE A5: Conference-League DiD\n\n")
cat("--- Panel A: Poisson DiD ---\n")
print(etable(m_did_fat_full, m_did_fat_nm, m_did_tra_full,
             headers = c("Fatigue full", "Fatigue near-miss", "Trauma full (placebo)"),
             se.below = TRUE, fitstat = ~ n + ll))
cat("\n--- Panel B: event study (ref = 2020/21) ---\n")
print(etable(m_event_fat, m_event_tra,
             headers = c("log Fatigue", "log Trauma (placebo)"),
             se.below = TRUE, fitstat = ~ n + r2))
sink()
cat("=== A5 written ===\n")

# ============================================================================ #
# TABLE A6: Deployment intensity -- GK-excluded & by-position (FUTURE WORK)      #
# ============================================================================ #
# Two further robustness cuts on the deployment-intensity ratio are PLANNED but
# intentionally NOT run here:
#   (a) outfield-only ratio (goalkeepers excluded), benchmarked against the
#       top-10 most valuable outfield players; and
#   (b) the ratio split by position line (DEF/MID/FWD), each benchmarked against
#       its own best-k ceiling, to test whether the structural reading is
#       position-specific.
# Both require a player-level position field that the current injury data does
# not contain. Reinstate this section once positions are joined in from the
# Transfermarkt squad data. The paper's structural-not-strategic claim does not
# depend on A6 -- it rests on the Table 4 deployment-intensity null.
# ============================================================================ #

message("A6 (GK-excluded / by-position deployment cuts): deferred to future work; not run.")

cat("\n=== 04 COMPLETE: A1-A5 written to ", out_path, " ===\n", sep = "")
