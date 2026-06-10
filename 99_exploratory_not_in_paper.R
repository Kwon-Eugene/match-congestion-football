################################################################################
# 99_exploratory_not_in_paper.R
#
# NOT IN THE PAPER. Superseded or auxiliary analyses, kept so results are
# reproducible if a referee asks. Nothing here writes a numbered paper table.
#
# Contents:
#   - Match-level squad-depth heterogeneity (superseded by Table 3)
#   - Audience-quality: avg MV on the pitch (superseded by Table 4)
#   - Standalone value-weighted triplet (cost already in Tables 3/6)
#   - Deployment intensity: CORRECT log vs levels robustness for total_squad_value
#   - Noll-Scully league-season panel exploration (n=50, underpowered)
#   - Cross-tabs behind the CB bridge
#   - Descriptive plots
################################################################################

source("00_setup.R")
master     <- load_master()
cb_panel   <- read_csv(paste0(derived_path, "cb_league_season_panel.csv"), show_col_types = FALSE)

# The match-level block below additionally needs the private + raw tiers.
MATCH_LEVEL_OK <- PRIVATE_DATA_AVAILABLE && RAW_DATA_AVAILABLE
if (MATCH_LEVEL_OK) {
  match_full <- read_csv(paste0(private_path, "match_level_fifpro.csv"), show_col_types = FALSE)
  mapping    <- read_csv(paste0(raw_path, "final_team_mapping2.csv"), show_col_types = FALSE)
}

mv  <- master %>% filter(!is.na(max_consec_congested_weeks), !is.na(total_squad_value))
mvg <- mv %>% filter(!is.na(mv_gini)) %>% mutate(log_avg_mv_per_minute = log(avg_mv_per_minute + 1))

# ============================================================================ #
# Match-level squad-depth heterogeneity (one sentence in the paper, not a table)#
# ============================================================================ #
# Caveat: Opponent_Rank is end-of-season final rank used as time-invariant.
if (!MATCH_LEVEL_OK) {
  message("Match-level squad-depth block skipped: requires data/private/ and data/raw/ (not distributed).")
} else {
  domestic_comps <- c("Serie A", "Premier League", "La Liga", "Ligue 1", "Bundesliga")
  opp_perf_map   <- mapping %>% select(Performance_Name, Congestion_Team_Name)
  opp_rank       <- master %>% select(Team, Season, League, League_Rank) %>%
    rename(Opponent_Rank = League_Rank) %>% distinct()

  match_lvl <- match_full %>%
    filter(!is.na(Team), Comp %in% domestic_comps) %>%
    mutate(
      Date           = as.Date(Date),
      Points         = case_when(Result == "W" ~ 3, Result == "D" ~ 1, Result == "L" ~ 0),
      GF_num         = as.numeric(GF), GA_num = as.numeric(GA), GD = GF_num - GA_num,
      Home           = as.integer(Venue == "Home"),
      iso_week       = lubridate::isoweek(Date),
      season_week    = as.factor(paste0(Season, "_W", sprintf("%02d", iso_week))),
      team_id        = as.factor(Team),
      season_id      = as.factor(Season),
      team_season_id = as.factor(paste0(Team, "_", Season))
    ) %>%
    filter(!is.na(Points), !is.na(Recovery_Days)) %>%
    left_join(opp_perf_map, by = c("Opponent" = "Congestion_Team_Name")) %>%
    rename(Opponent_Perf_Name = Performance_Name) %>%
    left_join(opp_rank, by = c("Opponent_Perf_Name" = "Team", "Season", "League")) %>%
    left_join(master %>% select(Team, Season, League, Num.Players, Av.Age,
                                european_team, post_5subs, total_matches),
              by = c("Team", "Season", "League"))

  m_match_squad <- feols(
    Points ~ Matches_L7D * Num.Players + Recovery_Days + Home + Opponent_Rank + Av.Age |
      team_id + season_id,
    data = match_lvl, vcov = ~team_id + season_week
  )
  cat("\n--- Match-level squad-depth interaction (superseded) ---\n")
  print(etable(m_match_squad, se.below = TRUE, fitstat = ~ n + r2 + ar2))
}

# ============================================================================ #
# Audience-quality: average MV on the pitch (superseded by deployment intensity)#
# ============================================================================ #
m_audq_consec <- feols(log_avg_mv_per_minute ~ max_consec_congested_weeks +
                         Num.Players + Av.Age | team_id + season_id,
                       data = mvg, vcov = ~team_id)
m_audq_het    <- feols(log_avg_mv_per_minute ~ max_consec_congested_weeks * high_mv_inequality +
                         Num.Players + Av.Age | team_id + season_id,
                       data = mvg, vcov = ~team_id)
cat("\n--- Audience-quality (superseded) ---\n")
print(etable(m_audq_consec, m_audq_het,
             headers = c("Consec", "Consec x HighIneq"), se.below = TRUE, fitstat = ~ n + r2))

# ============================================================================ #
# Standalone value-weighted triplet (cost outcome already in Tables 3 and 6)    #
# ============================================================================ #
m_fatcost <- feols(log_fatigue_cost ~ max_consec_congested_weeks + Num.Players + Av.Age +
                     post_5subs + covid_season | team_id + season_id, data = mv, vcov = ~team_id)
m_tracost <- feols(log_trauma_cost ~ max_consec_congested_weeks + Num.Players + Av.Age +
                     post_5subs + covid_season | team_id + season_id, data = mv, vcov = ~team_id)
cat("\n--- Value-weighted triplet (auxiliary) ---\n")
print(etable(m_fatcost, m_tracost,
             headers = c("log(Fatigue cost)", "log(Trauma cost)"),
             se.below = TRUE, fitstat = ~ n + r2))

# ============================================================================ #
# Deployment intensity: CORRECT log vs levels robustness                       #
# ============================================================================ #
# The old script compared two identical levels models mislabeled "log vs
# levels". Here log() is actually applied, so the contrast is real.
di <- mv %>% filter(!is.na(deployment_intensity), !is.na(high_mv_inequality))
m_di_log    <- feols(deployment_intensity ~ max_consec_congested_weeks * high_mv_inequality +
                       log_total_matches + log(total_squad_value + 1) +
                       Num.Players + Av.Age | team_id + season_id, data = di, vcov = ~team_id)
m_di_levels <- feols(deployment_intensity ~ max_consec_congested_weeks * high_mv_inequality +
                       log_total_matches + total_squad_value +
                       Num.Players + Av.Age | team_id + season_id, data = di, vcov = ~team_id)
cat("\n--- Deployment intensity: log vs levels squad value (correctly labeled) ---\n")
print(etable(m_di_log, m_di_levels,
             headers = c("log(value+1)", "value (levels)"),
             se.below = TRUE, fitstat = ~ n + r2))

# ============================================================================ #
# Noll-Scully panel exploration (n=50; descriptive only)                       #
# ============================================================================ #
cat("\n--- League-season CB panel summary ---\n")
print(cb_panel %>%
        group_by(League) %>%
        summarise(mean_noll_scully = round(mean(noll_scully_precise, na.rm = TRUE), 3),
                  mean_cr4 = round(mean(cr4, na.rm = TRUE), 3), .groups = "drop"))

# ============================================================================ #
# Cross-tabs behind the CB bridge                                              #
# ============================================================================ #
cat("\n--- european_team x high_mv_inequality (row proportions) ---\n")
print(round(prop.table(table(mvg$european_team, mvg$high_mv_inequality,
                             dnn = c("european", "high_ineq")), margin = 1), 3))

# ============================================================================ #
# Descriptive plots                                                            #
# ============================================================================ #
library(ggplot2)

league_trend <- master %>%
  group_by(League, Season) %>%
  summarise(Mean_Matches  = mean(total_matches, na.rm = TRUE),
            Mean_Recovery = mean(mean_recovery_days, na.rm = TRUE), .groups = "drop") %>%
  mutate(Season = factor(Season))

p_matches <- ggplot(league_trend, aes(Season, Mean_Matches, color = League, group = League)) +
  geom_line(linewidth = 1.1) + geom_point(size = 2) +
  theme_minimal(base_size = 13) +
  labs(title = "Mean matches per season by league", x = "Season", y = "Mean matches") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

p_gini <- ggplot(master, aes(log(total_squad_value + 1), mv_gini)) +
  geom_point(alpha = 0.5) + geom_smooth(method = "lm", se = FALSE) +
  theme_minimal(base_size = 13) +
  labs(title = "Squad value vs MV inequality", x = "log squad value", y = "MV Gini")

ggsave(paste0(out_path, "fig_matches_trend.png"), p_matches, width = 8, height = 5, dpi = 150)
ggsave(paste0(out_path, "fig_value_gini.png"),   p_gini,    width = 7, height = 5, dpi = 150)

cat("\n=== 99 COMPLETE ===\n")
