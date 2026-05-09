# =============================================================================
# 01_pull_cfbd.R
# Pull all available CFBD data for FBS seasons 2021–2023.
# Saves raw RDS checkpoints to data/raw/cfbd/ and loads into DuckDB.
#
# Run from nil_analytics/ directory:
#   Rscript scripts/01_pull_cfbd.R
#
# API key is expected as env var CFBD_API_KEY (set in run command, not here).
# Rate limit: 5000 requests/hour. This script uses ~50–80 calls total.
# =============================================================================

cat("=== NIL Analytics: CFBD Data Pull ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# -----------------------------------------------------------------------------
# 0. Package installation and loading
# -----------------------------------------------------------------------------
required_packages <- c(
  "cfbfastR", "tidyverse", "duckdb", "DBI", "digest", "lubridate"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}

suppressPackageStartupMessages({
  library(cfbfastR)
  library(tidyverse)
  library(duckdb)
  library(DBI)
  library(digest)
  library(lubridate)
})

cat("Packages loaded.\n\n")

# -----------------------------------------------------------------------------
# 1. API Key
# -----------------------------------------------------------------------------
api_key <- Sys.getenv("CFBD_API_KEY")
if (nchar(api_key) == 0) stop("CFBD_API_KEY env var not set. Aborting.")
cfbd_key(api_key)
cat("API key set.\n\n")

# -----------------------------------------------------------------------------
# 2. Paths and constants
# -----------------------------------------------------------------------------
SEASONS    <- 2021:2023
RAW_DIR    <- "data/raw/cfbd"
PROC_DIR   <- "data/processed"
DB_PATH    <- "db/nil_analytics.db"

dir.create(RAW_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(PROC_DIR, showWarnings = FALSE, recursive = TRUE)

# Helper: save RDS checkpoint with status message
save_checkpoint <- function(obj, name) {
  path <- file.path(RAW_DIR, paste0(name, ".rds"))
  saveRDS(obj, path)
  cat("  Saved:", path, paste0("(", nrow(obj), " rows)\n"))
  invisible(obj)
}

# Helper: load checkpoint if it exists (skip re-pull)
load_or_pull <- function(name, pull_fn) {
  path <- file.path(RAW_DIR, paste0(name, ".rds"))
  if (file.exists(path)) {
    cat("  Checkpoint found, loading:", path, "\n")
    return(readRDS(path))
  }
  cat("  Pulling:", name, "\n")
  result <- pull_fn()
  save_checkpoint(result, name)
  result
}

# -----------------------------------------------------------------------------
# 3. Connect to DuckDB
# -----------------------------------------------------------------------------
cat("Connecting to DuckDB:", DB_PATH, "\n")
con <- dbConnect(duckdb(), DB_PATH)
cat("Connected.\n\n")

# Cleanup on exit
on.exit({
  dbDisconnect(con, shutdown = TRUE)
  cat("\nDuckDB connection closed.\n")
}, add = TRUE)

# -----------------------------------------------------------------------------
# SECTION A: REFERENCE DATA — Conferences and Teams
# -----------------------------------------------------------------------------
cat("--- A. Reference Data ---\n")

# A1. Conferences
cat("A1. Conferences\n")
conf_raw <- load_or_pull("conferences", function() {
  cfbd_conferences()
})

# Deduplicate and load into DuckDB
conf_clean <- conf_raw %>%
  distinct(name, abbreviation, .keep_all = TRUE) %>%
  mutate(
    subdivision = case_when(
      abbreviation %in% c("ACC", "B12", "B1G", "Pac-12", "SEC",
                          "AAC", "CUSA", "MAC", "MWC", "SBC", "Ind") ~ "FBS",
      TRUE ~ "FCS"
    )
  ) %>%
  select(name, abbreviation, subdivision) %>%
  mutate(conference_id = row_number())

existing_confs <- dbGetQuery(con, "SELECT name FROM conferences")
new_confs <- conf_clean %>% filter(!name %in% existing_confs$name)
if (nrow(new_confs) > 0) {
  dbAppendTable(con, "conferences", new_confs %>% select(conference_id, name, abbreviation, subdivision))
  cat("  Inserted", nrow(new_confs), "conferences.\n")
} else {
  cat("  Conferences already loaded.\n")
}

# A2. Teams
cat("A2. Teams\n")
teams_raw <- load_or_pull("teams", function() {
  cfbd_team_info()
})

teams_clean <- teams_raw %>%
  filter(!is.na(school)) %>%
  distinct(school, .keep_all = TRUE) %>%
  select(school, mascot, abbreviation) %>%
  mutate(team_id = row_number())

existing_teams <- dbGetQuery(con, "SELECT school FROM teams")
new_teams <- teams_clean %>% filter(!school %in% existing_teams$school)
if (nrow(new_teams) > 0) {
  dbAppendTable(con, "teams", new_teams %>%
    select(team_id, school, mascot, abbreviation) %>%
    mutate(created_at = Sys.time()))
  cat("  Inserted", nrow(new_teams), "teams.\n")
} else {
  cat("  Teams already loaded.\n")
}

# Reload team lookup (including any previously inserted rows)
team_lookup <- dbGetQuery(con, "SELECT team_id, school FROM teams")

# A3. Team aliases — seed with CFBD names as the canonical source
cat("A3. Team aliases (CFBD source)\n")
cfbd_aliases <- team_lookup %>%
  mutate(source = "cfbd", alias_name = school) %>%
  select(team_id, source, alias_name)

existing_aliases <- dbGetQuery(con, "SELECT source, alias_name FROM team_aliases")
new_aliases <- cfbd_aliases %>%
  anti_join(existing_aliases, by = c("source", "alias_name")) %>%
  mutate(alias_id = row_number() + max(c(0,
    dbGetQuery(con, "SELECT COALESCE(MAX(alias_id),0) AS m FROM team_aliases")$m)))

if (nrow(new_aliases) > 0) {
  dbAppendTable(con, "team_aliases", new_aliases)
  cat("  Inserted", nrow(new_aliases), "aliases.\n")
} else {
  cat("  Aliases already loaded.\n")
}

# A4. Conference memberships (current — from team info)
cat("A4. Conference memberships\n")
conf_db  <- dbGetQuery(con, "SELECT conference_id, name FROM conferences")

memberships_raw <- teams_raw %>%
  filter(!is.na(school), !is.na(conference)) %>%
  left_join(team_lookup, by = "school") %>%
  left_join(conf_db, by = c("conference" = "name")) %>%
  filter(!is.na(team_id), !is.na(conference_id)) %>%
  mutate(
    division   = NA_character_,
    season_from = min(SEASONS),
    season_to   = NA_integer_
  ) %>%
  distinct(team_id, conference_id, season_from, .keep_all = TRUE) %>%
  select(team_id, conference_id, division, season_from, season_to) %>%
  mutate(membership_id = row_number())

existing_mem <- dbGetQuery(con, "SELECT team_id, season_from FROM conference_memberships")
new_mem <- memberships_raw %>%
  anti_join(existing_mem, by = c("team_id", "season_from"))

if (nrow(new_mem) > 0) {
  dbAppendTable(con, "conference_memberships", new_mem)
  cat("  Inserted", nrow(new_mem), "memberships.\n")
} else {
  cat("  Memberships already loaded.\n")
}

cat("\n")

# -----------------------------------------------------------------------------
# SECTION B: GAME METADATA
# -----------------------------------------------------------------------------
cat("--- B. Game Metadata ---\n")

games_all <- load_or_pull("games_all", function() {
  map_dfr(SEASONS, function(yr) {
    cat("    Season:", yr, "\n")
    bind_rows(
      cfbd_game_info(year = yr, season_type = "regular") %>% mutate(game_type = "regular"),
      cfbd_game_info(year = yr, season_type = "postseason") %>% mutate(game_type = "bowl")
    ) %>% mutate(season = yr)
  })
})

# Normalize team names → team_ids
alias_lookup <- dbGetQuery(con,
  "SELECT alias_name, team_id FROM team_aliases WHERE source = 'cfbd'")

games_clean <- games_all %>%
  left_join(alias_lookup, by = c("home_team" = "alias_name")) %>%
  rename(home_team_id = team_id) %>%
  left_join(alias_lookup, by = c("away_team" = "alias_name")) %>%
  rename(away_team_id = team_id) %>%
  filter(!is.na(home_team_id), !is.na(away_team_id)) %>%
  transmute(
    game_id      = row_number(),
    cfbd_game_id = as.integer(id),
    season,
    week         = as.integer(week),
    game_type,
    game_date    = as.Date(start_date),
    home_team_id = as.integer(home_team_id),
    away_team_id = as.integer(away_team_id),
    home_score   = as.integer(home_points),
    away_score   = as.integer(away_points),
    neutral_site = as.logical(neutral_site),
    conference_game = as.logical(conference_game),
    attendance   = as.integer(attendance),
    venue        = venue
  )

existing_games <- dbGetQuery(con, "SELECT cfbd_game_id FROM games")
new_games <- games_clean %>% filter(!cfbd_game_id %in% existing_games$cfbd_game_id)

if (nrow(new_games) > 0) {
  dbAppendTable(con, "games", new_games %>% mutate(created_at = Sys.time()))
  cat("  Inserted", nrow(new_games), "games.\n")
} else {
  cat("  Games already loaded.\n")
}

# Save internal game_id ↔ cfbd_game_id mapping for play joins
game_id_map <- dbGetQuery(con, "SELECT game_id, cfbd_game_id FROM games")
cat("\n")

# -----------------------------------------------------------------------------
# SECTION C: SP+ RATINGS
# -----------------------------------------------------------------------------
cat("--- C. SP+ Ratings ---\n")

sp_all <- load_or_pull("sp_all", function() {
  map_dfr(SEASONS, function(yr) {
    cat("    Season:", yr, "\n")
    cfbd_ratings_sp(year = yr) %>% mutate(season = yr)
  })
})

sp_clean <- sp_all %>%
  left_join(alias_lookup, by = c("team" = "alias_name")) %>%
  filter(!is.na(team_id)) %>%
  transmute(
    team_id         = as.integer(team_id),
    season          = as.integer(season),
    sp_overall      = as.double(rating),
    sp_offense      = as.double(offense.rating),
    sp_defense      = as.double(defense.rating),
    sp_special_teams = as.double(special_teams.rating)
  ) %>%
  distinct(team_id, season, .keep_all = TRUE) %>%
  mutate(sp_id = row_number())

existing_sp <- dbGetQuery(con, "SELECT team_id, season FROM sp_ratings")
new_sp <- sp_clean %>% anti_join(existing_sp, by = c("team_id", "season"))

if (nrow(new_sp) > 0) {
  dbAppendTable(con, "sp_ratings", new_sp)
  cat("  Inserted", nrow(new_sp), "SP+ records.\n")
} else {
  cat("  SP+ ratings already loaded.\n")
}
cat("\n")

# -----------------------------------------------------------------------------
# SECTION D: PLAY-BY-PLAY
# Pull week-by-week to stay within memory limits and enable checkpointing.
# ~15 calls/season × 3 seasons = ~45 API calls total.
# -----------------------------------------------------------------------------
cat("--- D. Play-by-Play ---\n")

pull_pbp_season_week <- function(yr, wk, season_type = "regular") {
  tryCatch({
    cfbd_pbp_data(
      year        = yr,
      week        = wk,
      season_type = season_type,
      epa_wpa     = TRUE
    )
  }, error = function(e) {
    cat("    WARNING: Failed week", wk, "season", yr, "-", conditionMessage(e), "\n")
    NULL
  })
}

for (yr in SEASONS) {
  season_checkpoint <- file.path(RAW_DIR, paste0("pbp_", yr, ".rds"))

  if (file.exists(season_checkpoint)) {
    cat("  PBP", yr, "- checkpoint found, skipping pull.\n")
    pbp_season <- readRDS(season_checkpoint)
  } else {
    cat("  PBP", yr, "- pulling regular season weeks 1–15...\n")
    pbp_regular <- map_dfr(1:15, function(wk) {
      cat("    Week", wk, "\r")
      Sys.sleep(0.3)   # polite pacing
      pull_pbp_season_week(yr, wk, "regular")
    })
    cat("\n")

    cat("  PBP", yr, "- pulling postseason...\n")
    pbp_post <- pull_pbp_season_week(yr, 1, "postseason")

    pbp_season <- bind_rows(pbp_regular, pbp_post) %>% mutate(season = yr)
    saveRDS(pbp_season, season_checkpoint)
    cat("  Saved:", season_checkpoint, paste0("(", nrow(pbp_season), " plays)\n"))
  }

  # --- Load PBP season into DuckDB plays table ---
  cat("  Loading PBP", yr, "into DuckDB...\n")

  plays_clean <- pbp_season %>%
    filter(!is.na(game_id)) %>%
    left_join(game_id_map, by = c("game_id" = "cfbd_game_id")) %>%
    left_join(alias_lookup, by = c("offense" = "alias_name")) %>%
    rename(offense_team_id = team_id) %>%
    left_join(alias_lookup, by = c("defense" = "alias_name")) %>%
    rename(defense_team_id = team_id) %>%
    filter(!is.na(game_id.y), !is.na(offense_team_id), !is.na(defense_team_id)) %>%
    transmute(
      cfbd_play_id    = as.integer(id),
      game_id         = as.integer(game_id.y),
      season          = as.integer(season),
      week            = as.integer(week),
      offense_team_id = as.integer(offense_team_id),
      defense_team_id = as.integer(defense_team_id),
      play_number     = as.integer(play_number),
      period          = as.integer(period),
      clock_minutes   = as.integer(clock.minutes),
      clock_seconds   = as.integer(clock.seconds),
      yard_line       = as.integer(yard_line),
      down            = as.integer(down),
      distance        = as.integer(distance),
      play_type       = as.character(play_type),
      yards_gained    = as.integer(yards_gained),
      scoring         = as.logical(scoring),
      score_offense   = as.integer(offense_score),
      score_defense   = as.integer(defense_score),
      epa             = as.double(EPA),
      wpa             = as.double(wp_added),
      success         = as.logical(success),
      garbage_time    = as.logical(garbage_time)
    ) %>%
    distinct(cfbd_play_id, .keep_all = TRUE) %>%
    mutate(play_id = row_number() +
             dbGetQuery(con, "SELECT COALESCE(MAX(play_id),0) AS m FROM plays")$m)

  existing_plays <- dbGetQuery(con,
    paste0("SELECT cfbd_play_id FROM plays WHERE season = ", yr))
  new_plays <- plays_clean %>%
    filter(!cfbd_play_id %in% existing_plays$cfbd_play_id)

  if (nrow(new_plays) > 0) {
    # Insert in chunks to avoid memory issues
    chunk_size <- 50000
    chunks     <- ceiling(nrow(new_plays) / chunk_size)
    for (i in seq_len(chunks)) {
      idx   <- ((i-1)*chunk_size + 1) : min(i*chunk_size, nrow(new_plays))
      dbAppendTable(con, "plays", new_plays[idx, ] %>%
        mutate(created_at = Sys.time()))
      cat("    Chunk", i, "/", chunks, "inserted.\n")
    }
    cat("  Inserted", nrow(new_plays), "plays for", yr, "\n")
  } else {
    cat("  Plays for", yr, "already loaded.\n")
  }
}
cat("\n")

# -----------------------------------------------------------------------------
# SECTION E: COMPUTE GAME-TEAM STATS from plays
# Aggregates EPA per team-game with garbage time filtered out.
# -----------------------------------------------------------------------------
cat("--- E. Computing game_team_stats from plays ---\n")

gts_query <- "
  SELECT
    game_id,
    season,
    offense_team_id                                          AS team_id,
    COUNT(*)  FILTER (WHERE play_type = 'Rush')              AS rush_plays,
    SUM(epa)  FILTER (WHERE play_type = 'Rush')              AS rush_epa_total,
    AVG(epa)  FILTER (WHERE play_type = 'Rush')              AS rush_epa_per_play,
    AVG(CASE WHEN play_type = 'Rush' AND success THEN 1.0 ELSE 0.0 END)
                                                             AS rush_success_rate,
    SUM(yards_gained) FILTER (WHERE play_type = 'Rush')      AS rush_yards,
    AVG(yards_gained) FILTER (WHERE play_type = 'Rush')      AS rush_yards_per_carry,
    COUNT(*)  FILTER (WHERE play_type IN (
      'Pass Reception','Pass Incompletion',
      'Pass Interception Return','Sack'))                    AS pass_plays,
    SUM(epa)  FILTER (WHERE play_type IN (
      'Pass Reception','Pass Incompletion',
      'Pass Interception Return','Sack'))                    AS pass_epa_total,
    AVG(epa)  FILTER (WHERE play_type IN (
      'Pass Reception','Pass Incompletion',
      'Pass Interception Return','Sack'))                    AS pass_epa_per_play,
    AVG(CASE WHEN play_type IN (
      'Pass Reception','Pass Incompletion',
      'Pass Interception Return','Sack')
      AND success THEN 1.0 ELSE 0.0 END)                    AS pass_success_rate,
    SUM(yards_gained) FILTER (WHERE play_type IN (
      'Pass Reception','Pass Incompletion',
      'Pass Interception Return','Sack'))                    AS pass_yards,
    AVG(epa)                                                 AS total_epa,
    AVG(epa) FILTER (WHERE success = TRUE)                   AS explosiveness
  FROM plays
  WHERE
    garbage_time = FALSE OR garbage_time IS NULL
    AND epa IS NOT NULL
    AND play_type IN (
      'Rush','Pass Reception','Pass Incompletion',
      'Pass Interception Return','Sack'
    )
  GROUP BY game_id, season, offense_team_id
  HAVING
    COUNT(*) FILTER (WHERE play_type = 'Rush') >= 5
    AND COUNT(*) FILTER (WHERE play_type IN (
      'Pass Reception','Pass Incompletion',
      'Pass Interception Return','Sack')) >= 5
"

gts_raw <- dbGetQuery(con, gts_query)

# Join is_home from games table
games_sides <- dbGetQuery(con, "SELECT game_id, home_team_id, away_team_id FROM games")

gts_clean <- gts_raw %>%
  left_join(games_sides, by = "game_id") %>%
  mutate(
    is_home  = team_id == home_team_id,
    gts_id   = row_number(),
    havoc_rate = NA_real_   # populated later when defensive stats available
  ) %>%
  select(
    gts_id, game_id, team_id, season, is_home,
    rush_plays, rush_epa_total, rush_epa_per_play, rush_success_rate,
    rush_yards, rush_yards_per_carry,
    pass_plays, pass_epa_total, pass_epa_per_play, pass_success_rate, pass_yards,
    total_epa, explosiveness, havoc_rate
  ) %>%
  filter(!is.na(is_home))

# Clear and reload (re-derived from full plays table)
dbExecute(con, "DELETE FROM game_team_stats")
dbAppendTable(con, "game_team_stats",
  gts_clean %>% mutate(created_at = Sys.time()))
cat("  Inserted", nrow(gts_clean), "game-team-stat rows.\n\n")

# -----------------------------------------------------------------------------
# SECTION F: SUMMARY
# -----------------------------------------------------------------------------
cat("=== Pull Complete ===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

summary_tables <- c("conferences", "teams", "team_aliases",
                    "conference_memberships", "games", "sp_ratings",
                    "plays", "game_team_stats")

for (tbl in summary_tables) {
  n <- dbGetQuery(con, paste0("SELECT COUNT(*) AS n FROM ", tbl))$n
  cat(sprintf("  %-26s %s rows\n", tbl, format(n, big.mark = ",")))
}

cat("\nModel-ready row count (v_model_ready):\n")
model_n <- dbGetQuery(con, "
  SELECT COUNT(*) AS n FROM v_model_ready
  WHERE rush_plays >= 10 AND pass_plays >= 10
    AND sp_rating IS NOT NULL
    AND opp_sp_rating IS NOT NULL
")$n
cat(sprintf("  %-26s %s rows\n", "v_model_ready (filtered)", format(model_n, big.mark = ",")))
cat("\nData pull complete. DuckDB ready at: db/nil_analytics.db\n")
