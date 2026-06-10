# Fixture Congestion, Player Welfare, and Competitive Balance

Reproducible analysis pipeline for a study of fixture congestion in Top-Five
European football: 976 club-seasons (England, Spain, Germany, Italy, France),
2015/16–2024/25. Congestion is measured as the longest run of **consecutive
congested weeks** (≥2 matches in a rolling 7-day window), aligned with FIFPRO's
medical guidance. The paper documents a fatigue-injury cost of congestion that
concentrates in squads with high internal market-value inequality, shows the
mechanism is structural (squad construction) rather than strategic (minute
allocation), finds no offsetting performance gain, and links congestion to a
compression of competitive balance among the most-exposed clubs.

*Working paper; full citation to be added.*

## Data availability

Source data were collected from [FBref](https://fbref.com) (fixtures, recovery
intervals, team performance) and [Transfermarkt](https://www.transfermarkt.com)
(injuries, player market values). **Per those sites' terms of use, the scraped
source files and player-/match-level derivatives are not redistributed here.**

What this repository ships instead is the **aggregated team-season panel**
(`data/derived/master_analysis.csv`, 976 rows × team-season aggregates and
author-constructed measures, plus a league-season competitive-balance panel).
Every table and figure in the paper — Tables 1–6 and Appendix A1–A5 — is
reproducible from the derived panel alone. Researchers who wish to rebuild the
panel from source can collect the equivalent inputs from FBref and Transfermarkt
and run `01_data_construction.R`; the script documents the expected files.

## Repository structure

```
├── 00_setup.R                     # libraries, paths, parameters, helpers
├── 01_data_construction.R        # raw sources -> derived panel  (needs data/raw/, not distributed)
├── 02_descriptives.R             # Table 1
├── 03_main_tables.R              # Tables 2–6
├── 04_appendix.R                 # Tables A1–A5 (A6 stubbed as future work)
├── 99_exploratory_not_in_paper.R # auxiliary analyses + figures
├── data/
│   ├── raw/                      # scraped sources        (gitignored, not distributed)
│   ├── private/                  # near-source derivatives (gitignored, not distributed)
│   └── derived/                  # aggregated panel        (INCLUDED)
├── output/                       # regression tables and figures
└── congestion.Rproj
```

## Reproducing the results

Requirements: R (≥ 4.3) with `tidyverse`, `fixest`, `readxl`, `MASS`,
`conflicted`.

1. Clone the repository and open `congestion.Rproj` in RStudio (this sets the
   working directory), or `setwd()` to the repo root.
2. Run, in order:

```r
source("02_descriptives.R")   # Table 1
source("03_main_tables.R")    # Tables 2–6
source("04_appendix.R")       # Tables A1–A5
```

All outputs are written to `output/`. `01_data_construction.R` is only needed
to rebuild `data/derived/` from raw sources and will stop with an explanatory
message if the (non-distributed) raw files are absent. The shareable parts of
`99_exploratory_not_in_paper.R` run from the derived panel; its match-level
block is skipped automatically without the restricted tiers.

## Table-to-code map

| Paper | Script | Output file |
|---|---|---|
| **Table 1** descriptives, European vs non-European exposure | `02` | `table1_*.csv/.txt` |
| **Table 2** headline triplet (fatigue / trauma placebo / points) | `03` | `table2_headline_triplet.txt` |
| **Table 3** squad-inequality interaction (central result) | `03` | `table3_inequality_interaction.txt` |
| **Table 4** deployment intensity (structural mechanism) | `03` | `table4_deployment_intensity.txt` |
| **Table 5** competitive-balance compression | `03` | `table5_cb_compression.txt` |
| **Table 6** attributable fatigue cost (modular) | `03` | `table6_attributable_cost.csv/.txt` |
| **A1** alternative FIFPRO measures (rhythm vs count) | `04` | `tableA1_altfifpro.txt` |
| **A2** control build-up | `04` | `tableA2_control_buildup.txt` |
| **A3** selection (lagged outcome) | `04` | `tableA3_selection.txt` |
| **A4** deployment invariance (minutes shares) | `04` | `tableA4_deployment_shares.txt` |
| **A5** Conference-League difference-in-differences | `04` | `tableA5_did.txt` |

## Specification conventions

Injury counts: Poisson with **team + season** fixed effects. Points and
competitive balance: OLS with **team + league-season** fixed effects
(points-per-match is only comparable within a league-season). Standard errors
clustered by team throughout. Headline regressor:
`max_consec_congested_weeks`; talent control: `log(total_squad_value + 1)`;
exposure control: `log(total_matches + 1)`.

## License

Code is released under the MIT License (see `LICENSE`). Files in
`data/derived/` are provided for research replication; underlying source data
remain subject to the terms of FBref and Transfermarkt.
