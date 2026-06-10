################################################################################
# 03_main_tables.R
#
# Main-text tables. All injury counts use team + season FE (Poisson); points
# and competitive balance use team + league-season FE (an intentional
# asymmetry: points-per-match is only meaningful within a league-season).
# SEs clustered by team throughout.
#
# OUTPUTS:
#   table2_headline_triplet.txt        fatigue / trauma-placebo / points
#   table3_inequality_interaction.txt  squad-inequality moderation (Panel B)
#   table4_deployment_intensity.txt    structural-not-strategic test
#   table5_cb_compression.txt          pts_dev and rank_pct panels
#   table6_attributable_cost.csv/.txt  player-value-at-risk (modular, cuttable)
################################################################################

source("00_setup.R")
master <- load_master()

# Common estimation sample for the value-based models.
mv  <- master %>% filter(!is.na(max_consec_congested_weeks), !is.na(total_squad_value))
mvg <- mv %>% filter(!is.na(mv_gini))

# ============================================================================ #
# TABLE 2: Headline injury triplet (talent + exposure controls)                #
# ============================================================================ #
#   E[Fatigue_it] = exp( b*MCC + d1*log(SquadValue) + d2*log(Matches)
#                        + g*Num.Players + l*Av.Age + a_i + t_t )
#   Trauma identical (placebo). Points = OLS with league-season FE.
# ============================================================================ #

m_fat <- fepois(
  fatigue_injuries ~ max_consec_congested_weeks + log(total_squad_value + 1) +
    log_total_matches + Num.Players + Av.Age + post_5subs + covid_season |
    team_id + season_id,
  data = mv, vcov = ~team_id
)
m_tra <- fepois(
  trauma_injuries ~ max_consec_congested_weeks + log(total_squad_value + 1) +
    log_total_matches + Num.Players + Av.Age + post_5subs + covid_season |
    team_id + season_id,
  data = mv, vcov = ~team_id
)
m_pts <- feols(
  Pts_per_Match ~ max_consec_congested_weeks + log(total_squad_value + 1) +
    log_total_matches + Num.Players + Av.Age |
    team_id + league_season_id,
  data = mv, vcov = ~team_id
)

cat("\n=== TABLE 2: Headline triplet (talent + exposure) ===\n")
etable(m_fat, m_tra, m_pts,
       headers  = c("Fatigue (Poisson)", "Trauma (placebo)", "Points/Match (OLS)"),
       se.below = TRUE, fitstat = ~ n + r2)

sink(paste0(out_path, "table2_headline_triplet.txt"))
cat("TABLE 2: Headline triplet, max_consec with talent + exposure controls\n\n")
etable(m_fat, m_tra, m_pts,
       headers  = c("Fatigue (Poisson)", "Trauma (placebo)", "Points/Match (OLS)"),
       se.below = TRUE, fitstat = ~ n + r2)
sink()

# ============================================================================ #
# TABLE 3: Squad-inequality interaction (Panel B, + talent control)            #
# ============================================================================ #
#   Y = b1*MCC + b2*HighIneq + b3*(MCC x HighIneq)
#       + d1*log(SquadValue) + d2*log(Matches) + controls + FE
#   Report b1 (low-ineq), b3 (differential), b1+b3 (high-ineq).
#   Talent control added for consistency with Table 2 (decision logged).
# ============================================================================ #

m_het_fat_count <- fepois(
  fatigue_injuries ~ max_consec_congested_weeks * high_mv_inequality +
    log(total_squad_value + 1) + log_total_matches +
    Num.Players + Av.Age + post_5subs + covid_season |
    team_id + season_id,
  data = mvg, vcov = ~team_id
)
m_het_fat_cost <- feols(
  log_fatigue_cost ~ max_consec_congested_weeks * high_mv_inequality +
    log(total_squad_value + 1) + log_total_matches +
    Num.Players + Av.Age + post_5subs + covid_season |
    team_id + season_id,
  data = mvg, vcov = ~team_id
)
m_het_pts <- feols(
  Pts_per_Match ~ max_consec_congested_weeks * high_mv_inequality +
    log(total_squad_value + 1) + log_total_matches +
    Num.Players + Av.Age |
    team_id + league_season_id,
  data = mvg, vcov = ~team_id
)

cat("\n=== TABLE 3: Squad-inequality interaction (Panel B, + talent) ===\n")
etable(m_het_fat_count, m_het_fat_cost, m_het_pts,
       headers  = c("Fatigue (Poisson)", "log(Fatigue cost)", "Pts/Match"),
       se.below = TRUE, fitstat = ~ n + r2)
# Implied high-inequality fatigue effect (b1 + b3) for the text.
b1 <- coef(m_het_fat_count)["max_consec_congested_weeks"]
b3 <- coef(m_het_fat_count)["max_consec_congested_weeks:high_mv_inequality"]
cat("\nImplied fatigue effect, high-inequality squads (b1 + b3):",
    round(b1 + b3, 4), "\n")

sink(paste0(out_path, "table3_inequality_interaction.txt"))
cat("TABLE 3: Fatigue cost concentrates in high-MV-inequality squads\n")
cat("b1 = low-inequality effect; b3 = differential; b1+b3 = high-inequality\n\n")
etable(m_het_fat_count, m_het_fat_cost, m_het_pts,
       headers  = c("Fatigue (Poisson)", "log(Fatigue cost)", "Pts/Match"),
       se.below = TRUE, fitstat = ~ n + r2)
cat("\nImplied high-inequality fatigue effect (b1 + b3):", round(b1 + b3, 4), "\n")
sink()

# ============================================================================ #
# TABLE 4: Deployment intensity (structural, not strategic)                    #
# ============================================================================ #
#   DI = avg MV/min / mean MV of top-11. Full spec uses log(total_squad_value).
#   Null on MCC => congestion does not shift clubs toward concentrating on
#   stars; the mechanism is squad construction, not deployment choice.
# ============================================================================ #

di <- mv %>% filter(!is.na(deployment_intensity), !is.na(high_mv_inequality))

m_di_exp <- feols(
  deployment_intensity ~ max_consec_congested_weeks + log_total_matches +
    Num.Players + Av.Age | team_id + season_id,
  data = di, vcov = ~team_id
)
m_di_het <- feols(
  deployment_intensity ~ max_consec_congested_weeks * high_mv_inequality +
    log_total_matches + Num.Players + Av.Age | team_id + season_id,
  data = di, vcov = ~team_id
)
m_di_full <- feols(
  deployment_intensity ~ max_consec_congested_weeks * high_mv_inequality +
    log_total_matches + log(total_squad_value + 1) +
    Num.Players + Av.Age | team_id + season_id,
  data = di, vcov = ~team_id
)

cat("\n=== TABLE 4: Deployment intensity ===\n")
cat("Mean DI:", round(mean(di$deployment_intensity, na.rm = TRUE), 4),
    "| Median:", round(median(di$deployment_intensity, na.rm = TRUE), 4), "\n")
etable(m_di_exp, m_di_het, m_di_full,
       headers  = c("Consec + exposure", "Consec x HighIneq", "Full (+ talent)"),
       se.below = TRUE, fitstat = ~ n + r2)

sink(paste0(out_path, "table4_deployment_intensity.txt"))
cat("TABLE 4: Deployment intensity ratio (structural, not strategic)\n\n")
cat("DV = actual avg MV/min / mean MV of top-11 most valuable players\n")
cat("Mean:", round(mean(di$deployment_intensity, na.rm = TRUE), 4),
    "| Median:", round(median(di$deployment_intensity, na.rm = TRUE), 4), "\n\n")
etable(m_di_exp, m_di_het, m_di_full,
       headers  = c("Consec + exposure", "Consec x HighIneq", "Full (+ talent)"),
       se.below = TRUE, fitstat = ~ n + r2)
sink()

# ============================================================================ #
# TABLE 5: Competitive-balance compression (within league-season)              #
# ============================================================================ #
#   Dev = th*MCC + ps*TopQ + ph*(MCC x TopQ) + controls + team + LS FE
#   ph < 0 = the top quartile's advantage compresses with exposure.
# ============================================================================ #

cb_within <- master %>%
  group_by(League, Season) %>%
  mutate(
    league_mean_pts_pm = mean(Pts_per_Match, na.rm = TRUE),
    pts_dev      = Pts_per_Match - league_mean_pts_pm,
    pts_quartile = ntile(Pts_per_Match, 4),
    is_top_q     = as.integer(pts_quartile == 4),
    rank_pct     = (rank(Pts_per_Match) - 1) / (n() - 1)
  ) %>%
  ungroup() %>%
  filter(!is.na(max_consec_congested_weeks))

m_cb_consec <- feols(
  pts_dev ~ max_consec_congested_weeks * is_top_q + Num.Players + Av.Age |
    team_id + league_season_id,
  data = cb_within, vcov = ~team_id
)
m_rank_consec <- feols(
  rank_pct ~ max_consec_congested_weeks * is_top_q + Num.Players + Av.Age |
    team_id + league_season_id,
  data = cb_within, vcov = ~team_id
)

# Bridge stat: what share of top-quartile team-seasons are European-competing?
top_q_euro_share <- cb_within %>%
  filter(is_top_q == 1) %>%
  summarise(share_european = mean(european_team, na.rm = TRUE)) %>%
  pull(share_european)

cat("\n=== TABLE 5: CB compression ===\n")
etable(m_cb_consec, m_rank_consec,
       headers  = c("pts_dev x Top", "rank_pct x Top"),
       se.below = TRUE, fitstat = ~ n + r2)
cat("\nShare of top-quartile team-seasons that are European-competing:",
    round(top_q_euro_share, 3), "\n")

sink(paste0(out_path, "table5_cb_compression.txt"))
cat("TABLE 5: Competitive-balance compression (within league-season)\n\n")
etable(m_cb_consec, m_rank_consec,
       headers  = c("pts_dev x Top", "rank_pct x Top"),
       se.below = TRUE, fitstat = ~ n + r2)
cat("\nTop-quartile team-seasons that are European-competing:",
    round(top_q_euro_share, 3),
    "\n(this overlap is the bridge from compression-at-the-top to the elite)\n")
sink()

# ============================================================================ #
# TABLE 6: Attributable fatigue cost (player-value-at-risk) -- MODULAR         #
# ============================================================================ #
# Counterfactual fatigue cost with max_consec = 0, summed across team-seasons,
# using squad-type-specific coefficients.
#
# COEFFICIENT SOURCE -- A DECISION TO REVISIT:
#   The previously reported figure (~EUR 91B, ~76% in high-inequality European
#   clubs) was computed from the UNCONTROLLED count-Poisson interaction. Table 3
#   is now the controlled (talent + exposure) interaction. This block defaults
#   to the uncontrolled source so the reorg REPRODUCES the known figure; switch
#   `attrib_src` to m_het_fat_count for consistency with Table 3 (the headline
#   number WILL move). Flagged for the results review.
# ============================================================================ #

attrib_src <- fepois(   # uncontrolled interaction = reproduces the prior figure
  fatigue_injuries ~ max_consec_congested_weeks * high_mv_inequality +
    Num.Players + Av.Age + post_5subs + covid_season |
    team_id + season_id,
  data = mvg, vcov = ~team_id
)
# Consistency alternative (uncomment to use Table 3's controlled coefficients):
# attrib_src <- m_het_fat_count

a_main  <- coef(attrib_src)["max_consec_congested_weeks"]
a_inter <- coef(attrib_src)["max_consec_congested_weeks:high_mv_inequality"]
beta_low  <- a_main
beta_high <- a_main + a_inter

attrib <- mvg %>%
  filter(!is.na(fatigue_cost), fatigue_cost > 0, !is.na(high_mv_inequality)) %>%
  mutate(
    beta_used           = ifelse(high_mv_inequality == 1, beta_high, beta_low),
    counterfactual_cost = fatigue_cost / exp(beta_used * max_consec_congested_weeks),
    attributable_cost   = fatigue_cost - counterfactual_cost
  )

attrib_summary <- attrib %>%
  summarise(
    n_team_seasons        = n(),
    sum_observed_cost     = sum(fatigue_cost),
    sum_attributable_cost = sum(attributable_cost),
    pct_attributable      = sum(attributable_cost) / sum(fatigue_cost) * 100
  )
attrib_by_both <- attrib %>%
  group_by(high_mv_inequality, european_team) %>%
  summarise(
    n_team_seasons      = n(),
    sum_attributable    = sum(attributable_cost),
    pct_of_total_attrib = sum(attributable_cost) / sum(attrib$attributable_cost) * 100,
    .groups             = "drop"
  )

cat("\n=== TABLE 6: Attributable fatigue cost ===\n")
cat("Coefficients used  low:", round(beta_low, 4), " high:", round(beta_high, 4), "\n")
print(attrib_summary)
print(attrib_by_both)

write_csv(attrib_by_both, paste0(out_path, "table6_attributable_cost.csv"))
sink(paste0(out_path, "table6_attributable_cost.txt"))
cat("TABLE 6: Attributable fatigue cost (player-value-at-risk) -- MODULAR\n")
cat("Coefficient source: UNCONTROLLED interaction (reproduces prior figure).\n\n")
cat("Coefficients  low:", round(beta_low, 4), " high:", round(beta_high, 4), "\n\n")
print(attrib_summary)
cat("\nBy squad inequality x European status:\n")
print(attrib_by_both)
sink()

cat("\n=== 03 COMPLETE ===\n")

