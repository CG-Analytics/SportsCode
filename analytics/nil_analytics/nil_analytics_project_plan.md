# NIL Analytics Project: O-Line vs. D-Line EPA Impact
## Project Brief for Agentic Execution — Claude Code / Cowork

---

## 0. Context & Purpose

**Client:** Virginia Tech Football Front Office Analytics (job application work sample)  
**Question:** Should we allocate more NIL budget to the offensive line or the defensive line?  
**Deliverable:** A two-model regression analysis (run + pass) with a formal hypothesis test, clean visualizations, and a ~3-page front-office memo.  
**Timeline:** Two weekends maximum. Phase 1 (data + EDA) is the priority for weekend one. Phase 2 (modeling + output) for weekend two.

---

## 1. Hypothesis

> **H₁ (Alternative):** The snap-weighted PFF run-blocking grade of a team's starting offensive line has a larger standardized regression coefficient on offensive rushing EPA per play than the snap-weighted PFF run-stopping grade of the opposing defensive line does, after controlling for running back quality, overall team quality (SP+), and home/away status.

> **H₀ (Null):** The two coefficients are equal (β_oline = β_dline).

An equivalent pass hypothesis is tested in a parallel model using pass-blocking/pass-rushing grades and passing EPA.

The formal test is a **one-sided Wald test** on standardized coefficients.

---

## 2. Data Sources

### 2A. College Football Data (CFBD) — Free API
- **URL:** `https://api.collegefootballdata.com`
- **R Package:** `cfbfastR` (install via `install.packages("cfbfastR")`)
- **API Key:** Register free at collegefootballdata.com → set via `cfbd_key(key)` or env var `CFBD_API_KEY`
- **Endpoints needed:**
  - `cfbd_pbp_data(year, epa_wpa = TRUE)` — play-by-play with EPA per play
  - `cfbd_ratings_sp(year)` — SP+ team ratings (opponent adjustment)
  - `cfbd_game_info(year)` — game metadata (home/away, game_id, week)
  - `cfbd_game_team_stats(year)` — team-level box stats per game
  - `cfbd_metrics_ppa_games(year)` — game-level PPA/EPA summaries
- **Seasons:** 2021, 2022, 2023 (three full FBS regular seasons + bowls)
- **Scope:** All FBS teams (~130 teams)

### 2B. PFF College Football Grades — Subscription + Light Scraping
- **URL:** `https://www.pff.com/college` (requires PFF+ subscription)
- **Access level required:** PFF College Premium Stats+ (~$120/yr) for historical grade access going back to 2021
- **Data needed per team per season:**
  - **Offensive Line:** snap counts + run-blocking grade + pass-blocking grade for all OL
  - **Defensive Line:** snap counts + run-stop grade + pass-rush grade for all DL/interior DL
  - **Running Backs:** snap counts + rushing grade for top 2 RBs
  - **Quarterbacks:** overall grade + passing grade
- **Extraction method:** Manual CSV export from PFF's position grades table (filter by team, position, season) — repeat for OL, DL, RB, QB for each of 3 seasons. Expect ~2–3 hours of manual export work total.
- **Output format:** Save all raw exports as CSV files in `/data/raw/pff/` named `pff_ol_2021.csv`, `pff_dl_2022.csv`, etc.

### 2C. SP+ Ratings — Via CFBD API
- Already available through `cfbd_ratings_sp(year)` — no additional work required.

---

## 3. Technical Environment

```r
# Required packages — install all before starting
install.packages(c(
  "cfbfastR",
  "tidyverse",
  "lmtest",
  "sandwich",
  "car",
  "corrplot",
  "ggplot2",
  "broom",
  "scales",
  "glmnet",      # for Ridge regression
  "kableExtra",  # for formatted tables
  "patchwork"    # for combining plots
))

# Set CFBD API key
Sys.setenv(CFBD_API_KEY = "YOUR_KEY_HERE")
```

**Working directory structure:**
```
nil_analytics/
├── data/
│   ├── raw/
│   │   ├── cfbd/          # API pull outputs
│   │   └── pff/           # Manual PFF CSV exports
│   └── processed/
│       ├── model_run.rds
│       └── model_pass.rds
├── scripts/
│   ├── 01_pull_cfbd.R
│   ├── 02_process_pff.R
│   ├── 03_merge_datasets.R
│   ├── 04_eda.R
│   ├── 05_model_run.R
│   ├── 06_model_pass.R
│   └── 07_visualize.R
├── outputs/
│   ├── figures/
│   └── memo/
└── README.md
```

---

## 4. Phase 1 — Data Collection & Processing

### Agent Task 1A: Pull CFBD Data (Automated)

**Script:** `scripts/01_pull_cfbd.R`

Pull the following for seasons 2021, 2022, 2023. Write each to `/data/raw/cfbd/` as RDS or CSV.

```r
library(cfbfastR)
library(tidyverse)

seasons <- 2021:2023

# 1. Play-by-play with EPA
pbp_all <- map_dfr(seasons, function(yr) {
  cfbd_pbp_data(year = yr, epa_wpa = TRUE) %>%
    mutate(season = yr)
})
saveRDS(pbp_all, "data/raw/cfbd/pbp_all.rds")

# 2. SP+ ratings
sp_all <- map_dfr(seasons, function(yr) {
  cfbd_ratings_sp(year = yr) %>% mutate(season = yr)
})
saveRDS(sp_all, "data/raw/cfbd/sp_all.rds")

# 3. Game info (for home/away, game_id)
games_all <- map_dfr(seasons, function(yr) {
  cfbd_game_info(year = yr) %>% mutate(season = yr)
})
saveRDS(games_all, "data/raw/cfbd/games_all.rds")

# 4. Game-level PPA summaries
ppa_all <- map_dfr(seasons, function(yr) {
  cfbd_metrics_ppa_games(year = yr) %>% mutate(season = yr)
})
saveRDS(ppa_all, "data/raw/cfbd/ppa_all.rds")
```

**Expected output:** ~4 RDS files, each covering 3 seasons of FBS data.

---

### Agent Task 1B: Compute Game-Level EPA by Play Type (Automated)

**Script:** Append to `01_pull_cfbd.R` or new section.

From the play-by-play, compute per-game, per-team averages of:
- `rush_epa_per_play` — mean EPA on rushing plays (play_type == "Rush")
- `pass_epa_per_play` — mean EPA on passing plays (play_type == "Pass")
- `rush_success_rate` — % of rushes with EPA > 0
- `pass_success_rate` — % of passes with EPA > 0

```r
pbp_all <- readRDS("data/raw/cfbd/pbp_all.rds")

game_epa <- pbp_all %>%
  filter(!is.na(epa), !is.na(play_type)) %>%
  filter(play_type %in% c("Rush", "Pass Reception", "Pass Incompletion",
                           "Pass Interception Return", "Sack")) %>%
  mutate(
    is_rush = play_type == "Rush",
    is_pass = !is_rush,
    rush_epa = if_else(is_rush, epa, NA_real_),
    pass_epa = if_else(is_pass, epa, NA_real_)
  ) %>%
  group_by(season, game_id, offense) %>%
  summarise(
    rush_epa_per_play = mean(rush_epa, na.rm = TRUE),
    pass_epa_per_play = mean(pass_epa, na.rm = TRUE),
    rush_plays = sum(is_rush, na.rm = TRUE),
    pass_plays = sum(is_pass, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(team = offense)

saveRDS(game_epa, "data/raw/cfbd/game_epa.rds")
```

---

### Agent Task 1C: Process PFF Data (After Manual Export)

**Script:** `scripts/02_process_pff.R`

**Assumption:** PFF CSVs have been manually exported and placed in `data/raw/pff/`. Each file contains: player_name, team, position, season, snap_count, run_block_grade (or run_stop_grade for DL), pass_block_grade (or pass_rush_grade for DL).

```r
library(tidyverse)

process_pff_group <- function(file_pattern, grade_col, n_top = 5) {
  files <- list.files("data/raw/pff/", pattern = file_pattern, full.names = TRUE)
  map_dfr(files, read_csv) %>%
    filter(!is.na(snap_count), snap_count > 0) %>%
    group_by(team, season) %>%
    arrange(desc(snap_count)) %>%
    slice_max(snap_count, n = n_top) %>%
    summarise(
      snap_weighted_grade = sum(get(grade_col) * snap_count) / sum(snap_count),
      total_snaps = sum(snap_count),
      n_players = n(),
      .groups = "drop"
    )
}

# Offensive Line — top 5 by snaps, run blocking + pass blocking
ol_run <- process_pff_group("pff_ol_", "run_block_grade", n_top = 5) %>%
  rename(ol_run_grade = snap_weighted_grade)

ol_pass <- process_pff_group("pff_ol_", "pass_block_grade", n_top = 5) %>%
  rename(ol_pass_grade = snap_weighted_grade)

# Defensive Line — top 4 by snaps
dl_run <- process_pff_group("pff_dl_", "run_stop_grade", n_top = 4) %>%
  rename(dl_run_grade = snap_weighted_grade)

dl_pass <- process_pff_group("pff_dl_", "pass_rush_grade", n_top = 4) %>%
  rename(dl_pass_grade = snap_weighted_grade)

# Running Backs — top 2 by snaps
rb_grade <- process_pff_group("pff_rb_", "rushing_grade", n_top = 2) %>%
  rename(rb_grade = snap_weighted_grade)

# Quarterbacks — top 1 starter
qb_grade <- process_pff_group("pff_qb_", "overall_grade", n_top = 1) %>%
  rename(qb_grade = snap_weighted_grade)

# Save
pff_processed <- list(
  ol_run = ol_run, ol_pass = ol_pass,
  dl_run = dl_run, dl_pass = dl_pass,
  rb_grade = rb_grade, qb_grade = qb_grade
)
saveRDS(pff_processed, "data/processed/pff_processed.rds")
```

**Note:** PFF grades are at the season level, not game level. Join season-level grades to game-level EPA in the merge step — each team's season grades will be the same across all games in that season.

---

### Agent Task 1D: Merge All Datasets

**Script:** `scripts/03_merge_datasets.R`

Join game-level EPA (offense-perspective) with:
- Home/away from game info (each game has two team perspectives)
- Opponent identity (to join opposing team's DL grade)
- Season-level PFF grades for both offense and defense
- SP+ ratings for overall quality control

```r
library(tidyverse)

game_epa   <- readRDS("data/raw/cfbd/game_epa.rds")
games_all  <- readRDS("data/raw/cfbd/games_all.rds")
sp_all     <- readRDS("data/raw/cfbd/sp_all.rds")
pff        <- readRDS("data/processed/pff_processed.rds")

# Build home/away and opponent columns
game_lookup <- games_all %>%
  select(game_id = id, season, home_team, away_team) %>%
  # Create two rows per game (one per team perspective)
  pivot_longer(c(home_team, away_team),
               names_to = "side", values_to = "team") %>%
  mutate(
    is_home = side == "home_team",
    opponent = if_else(side == "home_team", away_team, home_team)
  ) %>%
  select(game_id, season, team, opponent, is_home) %>%
  # Remove duplicate from pivot — needs game_id from original
  left_join(
    games_all %>% select(game_id = id, home_team, away_team),
    by = "game_id"
  ) %>%
  mutate(opponent = if_else(is_home, away_team, home_team)) %>%
  select(game_id, season, team, opponent, is_home)

# Join game EPA → home/away/opponent
model_base <- game_epa %>%
  left_join(game_lookup, by = c("game_id", "season", "team"))

# Join SP+ (offensive team)
sp_clean <- sp_all %>%
  select(team, season, sp_rating = rating,
         sp_offense = offense.rating, sp_defense = defense.rating)

model_base <- model_base %>%
  left_join(sp_clean, by = c("team", "season")) %>%
  left_join(sp_clean %>% rename_with(~paste0("opp_", .), -c(team, season)),
            by = c("opponent" = "team", "season"))

# Join PFF grades — offensive team
model_base <- model_base %>%
  left_join(pff$ol_run   %>% select(team, season, ol_run_grade),   by = c("team", "season")) %>%
  left_join(pff$ol_pass  %>% select(team, season, ol_pass_grade),  by = c("team", "season")) %>%
  left_join(pff$rb_grade %>% select(team, season, rb_grade),       by = c("team", "season")) %>%
  left_join(pff$qb_grade %>% select(team, season, qb_grade),       by = c("team", "season"))

# Join PFF grades — opposing defensive team
model_base <- model_base %>%
  left_join(pff$dl_run  %>% select(team, season, dl_run_grade)  %>% rename(opponent = team),
            by = c("opponent", "season")) %>%
  left_join(pff$dl_pass %>% select(team, season, dl_pass_grade) %>% rename(opponent = team),
            by = c("opponent", "season"))

# Filter: drop games with missing key variables
# Keep only FBS-vs-FBS games (non-FBS opponents lack SP+)
model_final <- model_base %>%
  filter(
    !is.na(rush_epa_per_play),
    !is.na(pass_epa_per_play),
    !is.na(ol_run_grade),
    !is.na(dl_run_grade),
    !is.na(sp_rating),
    !is.na(opp_sp_rating),
    rush_plays >= 10,
    pass_plays >= 10
  )

saveRDS(model_final, "data/processed/model_final.rds")
cat("Modeling dataset rows:", nrow(model_final), "\n")
# Expected: ~4,000-5,000 rows
```

---

## 5. Phase 2 — Exploratory Data Analysis

**Script:** `scripts/04_eda.R`

### Required EDA Steps

1. **Distributions** — histograms of `rush_epa_per_play`, `pass_epa_per_play`, all PFF grades. Flag any extreme outliers (blowouts, garbage time).

2. **Correlation matrix** — all predictor variables. Look for VIF > 5 between:
   - `ol_run_grade` ↔ `rb_grade` (expect moderate correlation)
   - `ol_run_grade` ↔ `sp_offense` (expect moderate/high — address via SP+ control)
   - `dl_run_grade` ↔ `sp_defense` (same concern)

3. **Scatter plots** — `ol_run_grade` vs. `rush_epa_per_play`, `dl_run_grade` vs. `rush_epa_per_play` (labeled by conference). These become your first slide for the memo.

4. **Season/conference checks** — do G5 conferences behave differently from P4? Consider whether to include a G5 indicator variable.

5. **Missing data audit** — which teams are missing PFF data? If >15% of FBS missing, reconsider the PFF scraping strategy.

```r
library(tidyverse)
library(corrplot)
library(patchwork)

df <- readRDS("data/processed/model_final.rds")

# Correlation matrix
grade_vars <- df %>%
  select(ol_run_grade, dl_run_grade, rb_grade, qb_grade,
         sp_offense, sp_defense, opp_sp_defense, opp_sp_offense)
corrplot(cor(grade_vars, use = "pairwise.complete.obs"),
         method = "color", type = "upper", addCoef.col = "black",
         tl.cex = 0.7, number.cex = 0.7)
```

---

## 6. Phase 3 — Modeling

### Model A: Rushing EPA

**Script:** `scripts/05_model_run.R`

**Outcome variable:** `rush_epa_per_play`  
**Unit of analysis:** Team-game (one row per team per game, offense perspective)

**Standardize all continuous predictors to z-scores** before fitting:

```r
library(tidyverse)
library(lmtest)
library(sandwich)
library(car)

df <- readRDS("data/processed/model_final.rds")

# Standardize predictors
df_run <- df %>%
  filter(!is.na(rush_epa_per_play)) %>%
  mutate(across(
    c(ol_run_grade, dl_run_grade, rb_grade, sp_offense, opp_sp_defense),
    ~ scale(.)[,1],
    .names = "z_{.col}"
  ),
  is_home = as.numeric(is_home),
  season_f = factor(season)
  )

# Model specification
# Primary model: rush EPA ~ O-line + opposing D-line + controls
run_model <- lm(
  rush_epa_per_play ~
    z_ol_run_grade +      # KEY PREDICTOR (offensive side)
    z_dl_run_grade +      # KEY PREDICTOR (defensive side)
    z_rb_grade +          # Control: RB quality
    z_sp_offense +        # Control: overall offensive team quality
    z_opp_sp_defense +    # Control: opponent overall defensive quality
    is_home +             # Control: home field advantage
    season_f,             # Season fixed effects
  data = df_run
)

# Cluster standard errors by team
run_model_coef <- coeftest(run_model,
  vcov = vcovCL(run_model, cluster = ~team))

summary(run_model)
print(run_model_coef)

# Wald test: H0: β_ol_run = β_dl_run
# (note dl_run enters negatively — better opponent DL hurts offense)
# Re-parameterize: create |β_dl| and test against β_ol
linearHypothesis(run_model,
  "z_ol_run_grade = -z_dl_run_grade",
  vcov = vcovCL(run_model, cluster = ~team))
```

**Interpretation note:** The DL grade is from the *opposing* team's perspective — a higher opposing DL run-stop grade *hurts* your EPA. So the DL coefficient will be negative. When comparing magnitudes, compare |β_dl| to β_ol.

---

### Model B: Passing EPA

**Script:** `scripts/06_model_pass.R`

Mirror of Model A with:
- Outcome: `pass_epa_per_play`
- Key predictors: `z_ol_pass_grade`, `z_dl_pass_grade` (pass-rush)
- Additional control: `z_qb_grade`
- Same clustering and Wald test structure

---

### Robustness Checks (run after primary models)

1. **Ridge regression version** using `glmnet` — verify coefficient direction and relative magnitude hold under regularization
2. **Conference subgroup** — re-run Model A on P4 teams only; does the result hold?
3. **Year-by-year stability** — run Model A separately for 2021, 2022, 2023; do coefficients stay consistent?

---

## 7. Phase 4 — Visualization & Memo

**Script:** `scripts/07_visualize.R`

### Required Figures

1. **Figure 1: Coefficient Plot** — standardized coefficients from Model A and Model B with 95% confidence intervals. Highlight O-line and D-line bars in contrasting colors. This is your key result figure.

2. **Figure 2: Scatter — O-line Grade vs. Rush EPA** — team-season dots, colored by conference. Add a smooth trend line.

3. **Figure 3: Scatter — D-line Grade vs. Rush EPA** — same format as Figure 2 for comparison.

4. **Figure 4: Marginal Effect Comparison Bar Chart** — a simple bar chart showing the standardized coefficient magnitude for O-line vs. D-line in both models. This is your "punchline" visualization for the memo.

---

### Memo Template Structure

```
NIL ROI Memo: Offensive vs. Defensive Line Investment
Prepared for: Football Analytics Working Group
Date: [date]

1. EXECUTIVE SUMMARY (2 sentences)
   - O-line grades show a [X]% larger standardized impact on rushing EPA per play
     than opposing D-line grades (β=X vs β=X, p=0.XX, one-sided Wald test).
   - Data: 3 FBS seasons (2021-2023), ~[N] team-game observations.

2. METHODOLOGY (1 paragraph)
3. RESULTS — Run Game (figure + 2 paragraphs)
4. RESULTS — Pass Game (figure + 2 paragraphs)
5. LIMITATIONS (bullet list — be honest about observational data)
6. RECOMMENDATION (1 paragraph — the NIL dollar answer)
```

---

## 8. Key Decisions & Constraints Already Made

| Decision | Choice | Rationale |
|---|---|---|
| Language | R | cfbfastR ecosystem, personal comfort |
| Seasons | 2021–2023 | 3 years = ~4,500–5,000 game-obs, manageable scrape |
| Scope | All FBS | Maximize N, include G5 flag as control |
| PFF grades | Season-level, top-N by snaps | Game-level too granular for 2-weekend timeline |
| EPA source | CFBD play-by-play | Free API, widely validated |
| Opponent adjustment | SP+ as control variable | Cuts endogeneity, no re-implementation needed |
| Unit of analysis | Team-game (offense perspective) | ~35x more observations vs. team-season |
| Multicollinearity fix | Standardize + VIF check + Ridge robustness | Belt-and-suspenders approach |
| Standard errors | Clustered by team | Accounts for within-team correlation across games |

---

## 9. Known Risks & Mitigations

| Risk | Likelihood | Mitigation |
|---|---|---|
| PFF export takes longer than expected | Medium | Do PFF exports first, before coding anything |
| Team name mismatches between PFF and CFBD | High | Build a name-normalization lookup table early (CFBD uses "Ohio State", PFF may use "Ohio St") |
| High VIF in predictor correlation matrix | Medium | Drop or combine highly correlated predictors; use Ridge spec as robustness check |
| Insufficient N after filtering (< 3,000 rows) | Low | Relax missing-data filter; check if PFF coverage is the bottleneck |
| Wald test non-significant | Medium | Still publishable — null result with correct methodology is a valid answer |

---

## 10. Agentic Execution Notes for Claude Code

- **Overnight run priority:** Task 1A (CFBD pull) is fully automated and safe to run overnight. It will take ~15–30 minutes for 3 seasons of play-by-play.
- **Human-in-loop required:** Task 1C (PFF processing) cannot start until manual CSV exports are complete. Do not attempt to auto-populate PFF data.
- **Name normalization:** Before any joins, run a team-name audit. Print all unique team names from PFF CSVs and CFBD data side-by-side. Resolve mismatches before merging.
- **Checkpoints:** Save intermediate RDS files after every script. Do not chain all scripts into one run without checkpoints.
- **EDA before modeling:** Do not skip the EDA phase. The correlation matrix result should be reviewed by the analyst before model specifications are finalized.
- **Output folder:** All figures go to `outputs/figures/`, named `fig01_coef_plot.png` etc. at 300 DPI minimum.

---

*Additional data organization specifics and PFF export column naming conventions to be provided by analyst before Phase 1C begins.*
