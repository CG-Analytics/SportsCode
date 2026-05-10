# NIL Analytics — Build Progress Log
**Project:** Virginia Tech Football NIL ROI Analysis (O-Line vs. D-Line)  
**Last Updated:** 2026-05-10

---

## Current Status: Data Pipeline Complete, Awaiting Full Run

The database schema, data pull script, and environment are fully built and
ready. A clean re-run is queued for after terminal restart.

---

## What Has Been Built

### Environment
- **R 4.6.0** and **RStudio** installed on the machine.
- **DuckDB** installed (both R package and CLI at `/opt/homebrew/bin/duckdb`).
- **`~/.R/Makevars`** created to fix Apple Clang 14 / R 4.6.0 compiler
  incompatibility (`-std=gnu2x` / `-std=gnu++2b`).
- **`~/.claude/settings.json`** set to `bypassPermissions` — Claude operates
  autonomously on git, shell, and file operations.
- **`~/.zshrc`** fixed: API key now exported as `CFBD_API_KEY` (was `CFDB_api`
  with spaces, which broke env var loading).

### Database — `db/nil_analytics.db`
Fresh DuckDB file with all 14 tables and 2 views applied from `db/schema.sql`:

| Table | Purpose |
|---|---|
| `conferences` | Canonical conference records |
| `teams` | One row per FBS/FCS program |
| `team_aliases` | Cross-source name map (CFBD → our team_id; PFF names added later) |
| `conference_memberships` | Realignment-aware season ranges |
| `games` | Game metadata with home/away team FKs |
| `plays` | Play-by-play (~140K rows per season expected) |
| `game_team_stats` | Pre-aggregated EPA per team-game (derived from plays) |
| `sp_ratings` | SP+ per team per season |
| `players` | Canonical player records (for PFF phase) |
| `player_team_seasons` | Transfer/redshirt bridge table |
| `player_season_grades` | PFF snap-weighted grades (OL, DL, LB, RB, QB, WR, TE, DB) |
| `game_player_stats` | Future game-level PFF data |
| `v_team_conference` | View: resolves team → conference by season |
| `v_model_ready` | View: regression-ready shape with all grades + SP+ joined |

**Key schema decisions made:**
- `plays.cfbd_play_id` is `BIGINT` (not INTEGER) — CFBD play IDs exceed R's
  integer limit (~2.1B), requiring numeric storage.
- `LB` position group added to `player_season_grades` alongside OL/DL — covers
  run stop, pass rush, and coverage grades for linebackers.
- `team_aliases` is the translation layer for all multi-source name reconciliation.
  Every join from external source → internal DB goes through this table.
- Conference memberships are season-ranged (realignment-aware).

### Data Pull Script — `scripts/01_pull_cfbd.R`
Fully written, committed, and pushed (latest: commit `3f18dfb`).

**What it pulls:**
- Conferences, Teams, Team Aliases, Conference Memberships (reference data)
- Games: 2021–2025, regular + postseason, via `cfbd_game_info()`
- SP+ ratings: 2021–2025, via `cfbd_ratings_sp()`
- Game team stats: 2021–2025, looping through each FBS team (required — endpoint
  returns HTTP 400 without a team filter)
- Play-by-play: 2021–2025, week by week via `cfbd_pbp_data(epa_wpa=TRUE)`
- Computes `game_team_stats` table by aggregating plays in DuckDB (SQL)
- Prints play type audit and team name match audit at the end

**Key bugs fixed in this script:**
1. `cfbd_play_id` conversion: `as.integer()` → `as.numeric()` — integer overflow
   was silently converting 97% of play IDs to NA, causing only 4,335/139,772
   plays to insert for 2021.
2. `cfbd_game_team_stats()` requires a team parameter — now loops through all
   ~130 FBS teams per season (~650 API calls total for 5 seasons).
3. Column name corrections: `pos_team` / `def_pos_team` (not `offense`/`defense`),
   `id_play` (not `id`), `wk` (not `week`), game_id renamed before transmute.
4. `play_id_col` computed outside `transmute()` to avoid dplyr per-row scoping bug.
5. `tidyverse` replaced with individual packages (avoids `textshaping`/`ragg`
   system library requirement).
6. `Sys.setenv(CFBD_API_KEY=...)` instead of `cfbd_key()` (version-safe).
7. Team name collision fix in conference memberships: `select(school, conference)`
   before joining to avoid `team_id.x`/`team_id.y` ambiguity.

**Checkpoint pattern:** Each section saves an RDS to `data/raw/cfbd/`. On re-run,
the script loads from disk and skips the API call. PBP checkpoints are per-season
(`pbp_2021.rds`, etc.).

**Current checkpoints on disk:**
- `conferences.rds` ✓
- `teams.rds` ✓
- `pbp_2021.rds` ✓ (139,772 plays — will reprocess into DB on next run)
- `games_all.rds`, `sp_all.rds`, `game_stats_all.rds`, `ppa_all.rds` — deleted
  to force re-pull for 2024–2025 seasons.

---

## What Happened During Debugging

| Session | Problem | Fix |
|---|---|---|
| 1 | `cfbfastR` not on CRAN | Install from `sportsdataverse.r-universe.dev` |
| 1 | Apple Clang 14 `-std=gnu23` error | Created `~/.R/Makevars` with `-std=gnu2x` |
| 1 | `tidyverse` needs `harfbuzz`/`fribidi` | Replaced with individual packages |
| 2 | `cfbd_key()` argument error | Changed to `Sys.setenv(CFBD_API_KEY=...)` |
| 2 | `team_id` collision in memberships | Added `select(school, conference)` before join |
| 2 | `game_id` naming conflict | `rename(cfbd_game_id_raw = game_id)` before transmute |
| 2 | Wrong PBP team column names | `offense`/`defense` → `pos_team`/`def_pos_team` |
| 2 | Wrong PBP play ID column | `id` → `id_play` |
| 2 | `play_id_col` not found in transmute | Moved detection outside transmute block |
| 3 | Only 4,335/139,772 plays inserted | `as.integer()` overflow on large CFBD IDs → `as.numeric()` |
| 3 | `game_team_stats` HTTP 400 | Endpoint requires team filter — added FBS team loop |
| 3 | DuckDB UNIQUE index corruption | Deleted corrupted DB, recreated from `schema.sql` via CLI |
| 3 | `.zshrc` API key syntax broken | Fixed: `export CFBD_API_KEY="..."` |

---

## Git History

```
3f18dfb  Fix cfbd_play_id integer overflow; add game_team_stats team loop
753f62c  Initial commit
```

---

## Ready to Run

After terminal restart:
```bash
cd ~/Desktop/repos/SportsCode/analytics/nil_analytics
Rscript scripts/01_pull_cfbd.R
```

Expected duration: ~45–60 minutes (team loop for game stats + 4 seasons of PBP).
Expected plays: ~140K per season × 5 seasons ≈ 700K total rows in `plays`.

---

## What Is NOT Yet Built

- `02_process_pff.R` — blocked on manual PFF CSV export
- `03_merge_datasets.R`
- `04_eda.R`
- `05_model_run.R`
- `06_model_pass.R`
- `07_visualize.R`
- Memo template

The PFF export is the only human-in-the-loop requirement before modeling can begin.
