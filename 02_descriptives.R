################################################################################
# 02_descriptives.R
#
# Table 1: congestion exposure, European vs non-European clubs. Establishes the
# asymmetry that motivates the paper. Includes the Welch t-test on consecutive
# congested weeks as the formal test behind the asymmetry claim.
#
# OUTPUT:
#   table1_descriptive_euro_vs_noneuro.csv
#   table1_exposure_asymmetry_test.txt
################################################################################

source("00_setup.R")
master <- load_master()

# ============================================================================ #
# TABLE 1: descriptive comparison                                              #
# ============================================================================ #
table_01 <- master %>%
  filter(!is.na(max_consec_congested_weeks)) %>%
  mutate(Group = ifelse(european_team == 1, "European", "Non-European")) %>%
  group_by(Group) %>%
  summarise(
    N                     = n(),
    Mean_total_matches    = round(mean(total_matches), 1),
    Mean_recovery_days    = round(mean(mean_recovery_days), 2),
    Mean_v3d_count        = round(mean(n_fifpro_v3d), 2),
    Mean_v35d_count       = round(mean(n_fifpro_v35d), 2),
    Mean_v4d_count        = round(mean(n_fifpro_v4d), 2),
    Mean_max_consec_weeks = round(mean(max_consec_congested_weeks), 2),
    Share_addl_violation  = round(mean(fifpro_addl_violation), 3),
    Mean_total_injuries   = round(mean(total_injuries, na.rm = TRUE), 1),
    Mean_fatigue_injuries = round(mean(fatigue_injuries, na.rm = TRUE), 1),
    Mean_trauma_injuries  = round(mean(trauma_injuries, na.rm = TRUE), 1),
    Mean_pts_per_match    = round(mean(Pts_per_Match, na.rm = TRUE), 3),
    .groups = "drop"
  )
write_csv(table_01, paste0(out_path, "table1_descriptive_euro_vs_noneuro.csv"))

cat("\n=== TABLE 1: Descriptive comparison (Euro vs non-European) ===\n")
print(table_01)

# ============================================================================ #
# Exposure asymmetry: detail + Welch t-test                                    #
# ============================================================================ #
mc_by_euro <- master %>%
  filter(!is.na(max_consec_congested_weeks)) %>%
  group_by(european_team) %>%
  summarise(
    n                 = n(),
    mean_max_consec   = mean(max_consec_congested_weeks, na.rm = TRUE),
    median_max_consec = median(max_consec_congested_weeks, na.rm = TRUE),
    sd_max_consec     = sd(max_consec_congested_weeks, na.rm = TRUE),
    p25               = quantile(max_consec_congested_weeks, 0.25, na.rm = TRUE),
    p75               = quantile(max_consec_congested_weeks, 0.75, na.rm = TRUE),
    max_observed      = max(max_consec_congested_weeks, na.rm = TRUE),
    .groups           = "drop"
  ) %>%
  mutate(european_label = ifelse(european_team == 1, "European", "Non-European"))

t_result <- t.test(max_consec_congested_weeks ~ european_team,
                   data = master %>% filter(!is.na(max_consec_congested_weeks)))

sink(paste0(out_path, "table1_exposure_asymmetry_test.txt"))
cat("TABLE 1 (support): consecutive-congested-weeks asymmetry\n\n")
print(mc_by_euro)
cat("\nWelch t-test (European vs Non-European):\n")
cat("  Mean difference:", round(diff(t_result$estimate), 4), "\n")
cat("  t =", round(t_result$statistic, 3), ", df =", round(t_result$parameter, 1),
    ", p =", format.pval(t_result$p.value, digits = 3), "\n")
cat("  95% CI: [", round(t_result$conf.int[1], 4), ",",
    round(t_result$conf.int[2], 4), "]\n")
sink()

cat("\n=== 02 COMPLETE ===\n")

