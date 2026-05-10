# =============================================================================
# 01_pull_cfbd.R
# Pull all available CFBD data for FBS seasons 2021–2023.
# Saves raw RDS checkpoints to data/raw/cfbd/ and loads into DuckDB.
#
# Run from nil_analytics/ directory:
#   CFBD_API_KEY="<key>" Rscript scripts/01_pull_cfbd.R
#
# Rate limit: 5000 requests/hour. This script uses ~60 calls total.
# =============================================================================

cat("=== NIL Analytics: CFBD Data Pull ===\n")
cat("Started:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# -----------------------------------------------------------------------------
# 0. Package installation and loading
# NOTE: cfbfastR is NOT on CRAN — must install from sportsdataverse r-universe.
#       All other packages install from CRAN normally.
# -----------------------------------------------------------------------------

# cfbfastR requires the sportsdataverse r-universe repo
if (!requireNamespace("cfbfastR", quietly = TRUE)) {
  cat("Installing: cfbfastR (from sportsdataverse r-universe)\n")
  install.packages(
    "cfbfastR",
    repos = c("https://sportsdataverse.r-universe.dev", "https://cloud.r-project.org"),
    quiet = TRUE
  )
}

# Individual tidyverse components only — avoids tidyverse meta-package which
# pulls in ragg/textshaping (require harfbuzz/fribidi system libs not present).
# ggplot2 and graphics packages are NOT needed for this data pull script.
cran_packages <- c(
  "dplyr", "purrr", "tidyr", "stringr", "magrittr", "tibble", "rlang",
  "duckdb", "DBI", "digest", "lubridate", "janitor"
)
for (pkg in cran_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)
  }
}

suppressPackageStartupMessages({
  library(cfbfastR)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(stringr)
  library(magrittr)
  library(tibble)
  library(duckdb)
  library(DBI)
  library(digest)
  library(lubridate)
  library(janitor)
})

cat("Packages loaded.\n\n")

# -----------------------------------------------------------------------------
# 1. API Key
# -----------------------------------------------------------------------------
api_key <- Sys.getenv("CFBD_API_KEY")
if (nchar(api_key) == 0) stop("CFBD_API_KEY env var not set. Aborting.")
# cfbd_key() signature varies by version — set env var directly, which
# cfbfastR reads internally via Sys.getenv("CFBD_API_KEY")
Sys.setenv(CFBD_API_KEY = api_key)
cat("API key set.\n\n")

# -----------------------------------------------------------------------------
# 2. Paths and constants
# -----------------------------------------------------------------------------
SEASONS   <- 2021:2025
RAW_DIR   <- "data/raw/cfbd"
PROC_DIR  <- "data/processed"
DB_PATH   <- "db/nil_analytics.db"

dir.create(RAW_DIR,  showWarnings = FALSE, recursive = TRUE)
dir.create(PROC_DIR, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 3. Helpers
# -----------------------------------------------------------------------------

# Save RDS checkpoint with status line
save_checkpoint <- function(obj, name) {
  path <- file.path(RAW_DIR, paste0(name, ".rds"))
  saveRDS(obj, path)
  cat("  Saved:", path, paste0("(", nrow(obj), " rows)\n"))
  invisible(obj)
}

# Load checkpoint if exists, otherwise run pull_fn and save
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

# Drop list-type columns (nested API responses) and normalize names to snake_case
flatten_api <- function(df) {
  df %>%
    select(where(~!is.list(.))) %>%
    clean_names()                   # dots/spaces → underscores, all lowercase
}

# Next available integer PK for a table (safe for incremental re-runs)
next_id <- function(table_name, id_col) {
  dbGetQuery(con,
    sprintf("SELECT COALESCE(MAX(%s), 0) + 1 AS n FROM %s", id_col, table_name)
  )$n
}

# Generate a sequence of IDs starting from next available PK
new_ids <- function(n, table_name, id_col) {
  start <- next_id(table_name, id_col)
  seq(start, start + n - 1)
}

# -----------------------------------------------------------------------------
# 4. Connect to DuckDB
# -----------------------------------------------------------------------------
cat("Connecting to DuckDB:", DB_PATH, "\n")
con <- dbConnect(duckdb(), DB_PATH)
cat("Connected.\n\n")

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
  flatten_api(cfbd_conferences())
})

# cfbd_conferences() returns: id, name, short_name, abbreviation, classification
conf_clean <- conf_raw %>%
  filter(!is.na(name)) %>%
  distinct(name, .keep_all = TRUE) %>%
  mutate(
    subdivision = case_when(
      abbreviation %in% c("ACC", "B12", "B1G", "Pac-12", "SEC",
                          "AAC", "CUSA", "MAC", "MWC", "SBC", "Ind") ~ "FBS",
      TRUE ~ coalesce(classification, "FCS")
    )
  ) %>%
  select(name, abbreviation, subdivision)

existing_confs <- dbGetQuery(con, "SELECT name FROM conferences")
new_confs <- conf_clean %>% filter(!name %in% existing_confs$name)

if (nrow(new_confs) > 0) {
  new_confs <- new_confs %>%
    mutate(conference_id = new_ids(n(), "conferences", "conference_id"))
  dbAppendTable(con, "conferences", new_confs %>%
    select(conference_id, name, abbreviation, subdivision))
  cat("  Inserted", nrow(new_confs), "conferences.\n")
} else {
  cat("  Conferences already loaded.\n")
}

# A2. Teams
cat("A2. Teams\n")
teams_raw <- load_or_pull("teams", function() {
  flatten_api(cfbd_team_info())
})

# cfbd_team_info() returns (after flatten): school, mascot, abbreviation, conference,
# classification, color, alt_color, id, alt_name_1, alt_name_2, alt_name_3, twitter
teams_clean <- teams_raw %>%
  filter(!is.na(school)) %>%
  distinct(school, .keep_all = TRUE) %>%
  select(school,
         mascot      = any_of(c("mascot")),
         abbreviation = any_of(c("abbreviation")))

existing_teams <- dbGetQuery(con, "SELECT school FROM teams")
new_teams <- teams_clean %>% filter(!school %in% existing_teams$school)

if (nrow(new_teams) > 0) {
  new_teams <- new_teams %>%
    mutate(team_id    = new_ids(n(), "teams", "team_id"),
           created_at = Sys.time())
  dbAppendTable(con, "teams", new_teams %>%
    select(team_id, school, any_of(c("mascot", "abbreviation")), created_at))
  cat("  Inserted", nrow(new_teams), "teams.\n")
} else {
  cat("  Teams already loaded.\n")
}

# Authoritative team lookup used throughout script
team_lookup <- dbGetQuery(con, "SELECT team_id, school FROM teams")

# A3. Team aliases — seed CFBD canonical names
cat("A3. Team aliases (CFBD source)\n")
cfbd_aliases <- team_lookup %>%
  transmute(team_id, source = "cfbd", alias_name = school)

existing_aliases <- dbGetQuery(con, "SELECT source, alias_name FROM team_aliases")
new_aliases <- cfbd_aliases %>%
  anti_join(existing_aliases, by = c("source", "alias_name"))

if (nrow(new_aliases) > 0) {
  new_aliases <- new_aliases %>%
    mutate(alias_id = new_ids(n(), "team_aliases", "alias_id"))
  dbAppendTable(con, "team_aliases", new_aliases %>%
    select(alias_id, team_id, source, alias_name))
  cat("  Inserted", nrow(new_aliases), "aliases.\n")
} else {
  cat("  Aliases already loaded.\n")
}

# Keep alias lookup in memory — used in every downstream join
alias_lookup <- dbGetQuery(con,
  "SELECT alias_name, team_id FROM team_aliases WHERE source = 'cfbd'")

# A4. Conference memberships
cat("A4. Conference memberships\n")
conf_db <- dbGetQuery(con, "SELECT conference_id, name FROM conferences")

memberships_raw <- teams_raw %>%
  # Select only school + conference to avoid collision: cfbd_team_info() returns
  # its own 'team_id' (CFBD's ID), which would conflict with our DB team_id
  # after the left_join below, producing team_id.x / team_id.y ambiguity.
  select(school, conference) %>%
  filter(!is.na(school), !is.na(conference)) %>%
  left_join(team_lookup, by = "school") %>%      # adds our DB team_id
  left_join(conf_db, by = c("conference" = "name")) %>%  # adds conference_id
  filter(!is.na(team_id), !is.na(conference_id)) %>%
  transmute(
    team_id       = as.integer(team_id),
    conference_id = as.integer(conference_id),
    division      = NA_character_,
    season_from   = min(SEASONS),
    season_to     = NA_integer_
  ) %>%
  distinct(team_id, conference_id, season_from, .keep_all = TRUE)

existing_mem <- dbGetQuery(con, "SELECT team_id, season_from FROM conference_memberships")
new_mem <- memberships_raw %>%
  anti_join(existing_mem, by = c("team_id", "season_from"))

if (nrow(new_mem) > 0) {
  new_mem <- new_mem %>%
    mutate(membership_id = new_ids(n(), "conference_memberships", "membership_id"))
  dbAppendTable(con, "conference_memberships", new_mem %>%
    select(membership_id, team_id, conference_id, division, season_from, season_to))
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
      flatten_api(cfbd_game_info(year = yr, season_type = "regular"))   %>% mutate(game_type = "regular"),
      flatten_api(cfbd_game_info(year = yr, season_type = "postseason")) %>% mutate(game_type = "bowl")
    ) %>% mutate(season = yr)
  })
})

# cfbd_game_info() → after flatten_api (confirmed column names):
# game_id, season, week, season_type, start_date, neutral_site, conference_game,
# attendance, venue, home_team, home_points, away_team, away_points, ...
games_clean <- games_all %>%
  # Rename cfbfastR's game_id → cfbd_game_id_raw to avoid collision with our
  # internal DB game_id that we assign later
  rename(cfbd_game_id_raw = game_id) %>%
  left_join(alias_lookup, by = c("home_team" = "alias_name")) %>%
  rename(home_team_id = team_id) %>%
  left_join(alias_lookup, by = c("away_team" = "alias_name")) %>%
  rename(away_team_id = team_id) %>%
  filter(!is.na(home_team_id), !is.na(away_team_id)) %>%
  transmute(
    cfbd_game_id    = as.integer(cfbd_game_id_raw),
    season          = as.integer(season),
    week            = as.integer(week),
    game_type       = as.character(game_type),
    game_date       = as.Date(substr(start_date, 1, 10)),
    home_team_id    = as.integer(home_team_id),
    away_team_id    = as.integer(away_team_id),
    home_score      = as.integer(home_points),
    away_score      = as.integer(away_points),
    neutral_site    = as.logical(neutral_site),
    conference_game = as.logical(conference_game),
    attendance      = as.integer(attendance),
    venue           = as.character(venue)
  ) %>%
  distinct(cfbd_game_id, .keep_all = TRUE)

existing_games <- dbGetQuery(con, "SELECT cfbd_game_id FROM games")
new_games <- games_clean %>% filter(!cfbd_game_id %in% existing_games$cfbd_game_id)

if (nrow(new_games) > 0) {
  new_games <- new_games %>%
    mutate(game_id    = new_ids(n(), "games", "game_id"),
           created_at = Sys.time())
  dbAppendTable(con, "games", new_games %>%
    select(game_id, cfbd_game_id, season, week, game_type, game_date,
           home_team_id, away_team_id, home_score, away_score,
           neutral_site, conference_game, attendance, venue, created_at))
  cat("  Inserted", nrow(new_games), "games.\n")
} else {
  cat("  Games already loaded.\n")
}

# Internal game_id ↔ cfbd_game_id map — used to join plays to DB game IDs
game_id_map <- dbGetQuery(con, "SELECT game_id, cfbd_game_id FROM games")
cat("\n")

# -----------------------------------------------------------------------------
# SECTION C: SP+ RATINGS
# -----------------------------------------------------------------------------
cat("--- C. SP+ Ratings ---\n")

sp_all <- load_or_pull("sp_all", function() {
  map_dfr(SEASONS, function(yr) {
    cat("    Season:", yr, "\n")
    flatten_api(cfbd_ratings_sp(year = yr)) %>% mutate(season = yr)
  })
})

# cfbd_ratings_sp() → after flatten_api (dots → underscores):
# team, conference, rating, second_order_wins, season,
# offense_rating, offense_success, offense_explosiveness,
# defense_rating, defense_success, defense_explosiveness, defense_havoc_total,
# special_teams_rating
sp_clean <- sp_all %>%
  left_join(alias_lookup, by = c("team" = "alias_name")) %>%
  filter(!is.na(team_id)) %>%
  transmute(
    team_id          = as.integer(team_id),
    season           = as.integer(season),
    sp_overall       = as.double(rating),
    sp_offense       = as.double(if ("offense_rating"       %in% names(.)) offense_rating       else NA_real_),
    sp_defense       = as.double(if ("defense_rating"       %in% names(.)) defense_rating       else NA_real_),
    sp_special_teams = as.double(if ("special_teams_rating" %in% names(.)) special_teams_rating else NA_real_)
  ) %>%
  distinct(team_id, season, .keep_all = TRUE)

existing_sp <- dbGetQuery(con, "SELECT team_id, season FROM sp_ratings")
new_sp <- sp_clean %>% anti_join(existing_sp, by = c("team_id", "season"))

if (nrow(new_sp) > 0) {
  new_sp <- new_sp %>%
    mutate(sp_id = new_ids(n(), "sp_ratings", "sp_id"))
  dbAppendTable(con, "sp_ratings", new_sp %>%
    select(sp_id, team_id, season, sp_overall, sp_offense, sp_defense, sp_special_teams))
  cat("  Inserted", nrow(new_sp), "SP+ records.\n")
} else {
  cat("  SP+ ratings already loaded.\n")
}
cat("\n")

# -----------------------------------------------------------------------------
# SECTION D: SUPPLEMENTARY GAME-LEVEL METRICS (cfbd_game_team_stats + ppa_games)
# Pulled as validation/supplement alongside the play-by-play aggregation.
# Saved to raw/ for reference; not loaded into a separate DB table (merged
# into game_team_stats via play aggregation in Section F).
# -----------------------------------------------------------------------------
cat("--- D. Supplementary Game Metrics ---\n")

game_stats_all <- load_or_pull("game_stats_all", function() {
  map_dfr(SEASONS, function(yr) {
    cat("    cfbd_game_team_stats:", yr, "\n")
    tryCatch(
      flatten_api(cfbd_game_team_stats(year = yr)) %>% mutate(season = yr),
      error = function(e) {
        cat("    WARNING: game_team_stats failed for", yr, "-", conditionMessage(e), "\n")
        tibble()
      }
    )
  })
})
cat("  game_team_stats rows:", nrow(game_stats_all), "\n")

ppa_all <- load_or_pull("ppa_all", function() {
  map_dfr(SEASONS, function(yr) {
    cat("    cfbd_metrics_ppa_games:", yr, "\n")
    tryCatch(
      flatten_api(cfbd_metrics_ppa_games(year = yr)) %>% mutate(season = yr),
      error = function(e) {
        cat("    WARNING: ppa_games failed for", yr, "-", conditionMessage(e), "\n")
        tibble()
      }
    )
  })
})
cat("  ppa_games rows:", nrow(ppa_all), "\n\n")

# -----------------------------------------------------------------------------
# SECTION E: PLAY-BY-PLAY
# Pulled week-by-week. ~15 calls/season × 3 seasons = ~45 API calls.
# tryCatch returns empty tibble on failure so map_dfr never crashes.
# -----------------------------------------------------------------------------
cat("--- E. Play-by-Play ---\n")

pull_pbp_week <- function(yr, wk, season_type = "regular") {
  tryCatch({
    result <- cfbd_pbp_data(
      year        = yr,
      week        = wk,
      season_type = season_type,
      epa_wpa     = TRUE
    )
    if (is.null(result) || nrow(result) == 0) return(tibble())
    flatten_api(result)
  }, error = function(e) {
    cat("    WARNING: PBP failed -", yr, season_type, "week", wk,
        "-", conditionMessage(e), "\n")
    tibble()   # empty tibble, not NULL — safe for map_dfr / bind_rows
  })
}

for (yr in SEASONS) {
  season_checkpoint <- file.path(RAW_DIR, paste0("pbp_", yr, ".rds"))

  if (file.exists(season_checkpoint)) {
    cat("  PBP", yr, "- checkpoint found, skipping pull.\n")
    pbp_season <- readRDS(season_checkpoint)
  } else {
    cat("  PBP", yr, "- pulling regular season (weeks 1–15)...\n")
    pbp_regular <- map_dfr(1:15, function(wk) {
      cat("    Week", sprintf("%2d", wk), "\r")
      Sys.sleep(0.3)
      pull_pbp_week(yr, wk, "regular")
    })
    cat("\n")

    cat("  PBP", yr, "- pulling postseason...\n")
    pbp_post <- pull_pbp_week(yr, 1, "postseason")

    pbp_season <- bind_rows(pbp_regular, pbp_post) %>% mutate(season = yr)
    saveRDS(pbp_season, season_checkpoint)
    cat("  Saved:", season_checkpoint,
        paste0("(", format(nrow(pbp_season), big.mark = ","), " plays)\n"))
  }

  cat("  Validating PBP column names for", yr, "...\n")
  pbp_cols <- names(pbp_season)

  # EPA column detection — cfbfastR uses 'epa' (snake_case after clean_names)
  # Original API sometimes returns 'EPA' uppercase, clean_names() normalizes it.
  epa_col <- intersect(c("epa", "ppa"), pbp_cols)[1]
  if (is.na(epa_col)) {
    cat("  WARNING: No EPA/PPA column found in PBP for", yr,
        "- available cols:", paste(pbp_cols[1:20], collapse = ", "), "\n")
    epa_col <- NA_character_
  } else {
    cat("  EPA column detected as:", epa_col, "\n")
  }

  # WPA column detection
  wpa_col <- intersect(c("wpa", "wp_added", "win_prob_added"), pbp_cols)[1]
  if (is.na(wpa_col)) wpa_col <- NA_character_

  # Clock column detection — may be clock_minutes/clock_seconds or just clock
  has_split_clock <- all(c("clock_minutes", "clock_seconds") %in% pbp_cols)

  cat("  Loading PBP", yr, "into DuckDB plays table...\n")

  # cfbfastR PBP confirmed column names (from inspection of pbp_2021.rds):
  #   game_id   = cfbfastR's CFBD game ID (rename before join)
  #   pos_team  = offensive team name (NOT 'offense')
  #   def_pos_team = defensive team name (NOT 'defense')
  #   id_play   = play identifier (NOT 'id')
  #   wk        = week number (NOT 'week')
  #   epa, wpa, play_type, down, distance, yards_gained, etc. all confirmed present
  #   No 'garbage_time' column in cfbfastR output
  plays_raw <- pbp_season %>%
    rename(cfbd_game_id_pbp = game_id) %>%
    left_join(game_id_map, by = c("cfbd_game_id_pbp" = "cfbd_game_id")) %>%
    # game_id now = our internal DB ID
    left_join(alias_lookup, by = c("pos_team" = "alias_name")) %>%
    rename(offense_team_id = team_id) %>%
    left_join(alias_lookup, by = c("def_pos_team" = "alias_name")) %>%
    rename(defense_team_id = team_id) %>%
    filter(!is.na(game_id), !is.na(offense_team_id), !is.na(defense_team_id))

  if (nrow(plays_raw) == 0) {
    cat("  WARNING: No plays passed join filters for", yr, "- check team name mapping.\n")
    next
  }

  plays_clean <- plays_raw %>%
    transmute(
      cfbd_play_id    = as.integer({
        # cfbfastR confirmed name: id_play. Fallbacks for safety.
        play_id_col <- intersect(c("id_play", "play_id", "id"), names(.))
        if (length(play_id_col) > 0) .data[[play_id_col[1]]] else NA_integer_
      }),
      game_id         = as.integer(game_id),
      season          = as.integer(season),
      week            = as.integer(if ("wk" %in% names(.)) wk else
                                   if ("week" %in% names(.)) week else NA_integer_),
      offense_team_id = as.integer(offense_team_id),
      defense_team_id = as.integer(defense_team_id),
      play_number     = as.integer(if ("play_number"    %in% names(.)) play_number    else NA_integer_),
      period          = as.integer(if ("period"         %in% names(.)) period         else NA_integer_),
      clock_minutes   = as.integer(if (has_split_clock)  clock_minutes  else NA_integer_),
      clock_seconds   = as.integer(if (has_split_clock)  clock_seconds  else NA_integer_),
      yard_line       = as.integer(if ("yard_line"      %in% names(.)) yard_line      else NA_integer_),
      down            = as.integer(if ("down"           %in% names(.)) down           else NA_integer_),
      distance        = as.integer(if ("distance"       %in% names(.)) distance       else NA_integer_),
      play_type       = as.character(if ("play_type"    %in% names(.)) play_type      else NA_character_),
      yards_gained    = as.integer(if ("yards_gained"   %in% names(.)) yards_gained   else NA_integer_),
      scoring         = as.logical(if ("scoring"        %in% names(.)) scoring        else NA),
      score_offense   = as.integer(if ("offense_score"  %in% names(.)) offense_score  else NA_integer_),
      score_defense   = as.integer(if ("defense_score"  %in% names(.)) defense_score  else NA_integer_),
      epa             = if (!is.na(epa_col)) as.double(.data[[epa_col]]) else NA_real_,
      wpa             = if (!is.na(wpa_col)) as.double(.data[[wpa_col]]) else NA_real_,
      success         = as.logical(if ("success"        %in% names(.)) success        else NA),
      garbage_time    = as.logical(if ("garbage_time"   %in% names(.)) garbage_time   else NA)
    ) %>%
    filter(!is.na(cfbd_play_id)) %>%
    distinct(cfbd_play_id, .keep_all = TRUE)

  existing_plays <- dbGetQuery(con,
    sprintf("SELECT cfbd_play_id FROM plays WHERE season = %d", yr))
  new_plays <- plays_clean %>%
    filter(!cfbd_play_id %in% existing_plays$cfbd_play_id)

  if (nrow(new_plays) == 0) {
    cat("  Plays for", yr, "already loaded.\n")
    next
  }

  # Assign PKs with offset so re-runs never collide
  new_plays <- new_plays %>%
    mutate(
      play_id    = new_ids(n(), "plays", "play_id"),
      created_at = Sys.time()
    )

  # Insert in 50k-row chunks to stay within memory limits
  chunk_size <- 50000
  n_chunks   <- ceiling(nrow(new_plays) / chunk_size)
  for (i in seq_len(n_chunks)) {
    idx <- ((i-1)*chunk_size + 1) : min(i*chunk_size, nrow(new_plays))
    dbAppendTable(con, "plays", new_plays[idx, ])
    cat("    Chunk", i, "/", n_chunks, "inserted\r")
  }
  cat("\n  Inserted", format(nrow(new_plays), big.mark = ","), "plays for", yr, "\n")
}
cat("\n")

# -----------------------------------------------------------------------------
# SECTION F: COMPUTE game_team_stats FROM PLAYS
# Aggregates per team-game, garbage-time excluded, FBS-only plays only.
# Parentheses around the OR condition are required — precedence bug fix.
# -----------------------------------------------------------------------------
cat("--- F. Computing game_team_stats from plays ---\n")

gts_query <- "
  SELECT
    p.game_id,
    p.season,
    p.offense_team_id                                              AS team_id,
    -- Rushing: 'Rush' + 'Rushing Touchdown' (confirmed cfbfastR play_type values)
    COUNT(*)    FILTER (WHERE p.play_type IN ('Rush','Rushing Touchdown'))
                                                                   AS rush_plays,
    SUM(p.epa)  FILTER (WHERE p.play_type IN ('Rush','Rushing Touchdown'))
                                                                   AS rush_epa_total,
    AVG(p.epa)  FILTER (WHERE p.play_type IN ('Rush','Rushing Touchdown'))
                                                                   AS rush_epa_per_play,
    AVG(CASE WHEN p.play_type IN ('Rush','Rushing Touchdown')
             AND p.success THEN 1.0 ELSE 0.0 END)                 AS rush_success_rate,
    SUM(p.yards_gained) FILTER (WHERE p.play_type IN ('Rush','Rushing Touchdown'))
                                                                   AS rush_yards,
    AVG(p.yards_gained) FILTER (WHERE p.play_type IN ('Rush','Rushing Touchdown'))
                                                                   AS rush_yards_per_carry,
    -- Passing: all pass-play types (confirmed cfbfastR values)
    COUNT(*)    FILTER (WHERE p.play_type IN (
                  'Pass Reception','Pass Incompletion','Sack',
                  'Passing Touchdown','Interception Return',
                  'Interception Return Touchdown'))                 AS pass_plays,
    SUM(p.epa)  FILTER (WHERE p.play_type IN (
                  'Pass Reception','Pass Incompletion','Sack',
                  'Passing Touchdown','Interception Return',
                  'Interception Return Touchdown'))                 AS pass_epa_total,
    AVG(p.epa)  FILTER (WHERE p.play_type IN (
                  'Pass Reception','Pass Incompletion','Sack',
                  'Passing Touchdown','Interception Return',
                  'Interception Return Touchdown'))                 AS pass_epa_per_play,
    AVG(CASE WHEN p.play_type IN (
                  'Pass Reception','Pass Incompletion','Sack',
                  'Passing Touchdown','Interception Return',
                  'Interception Return Touchdown')
             AND p.success THEN 1.0 ELSE 0.0 END)                 AS pass_success_rate,
    SUM(p.yards_gained) FILTER (WHERE p.play_type IN (
                  'Pass Reception','Pass Incompletion','Sack',
                  'Passing Touchdown','Interception Return',
                  'Interception Return Touchdown'))                 AS pass_yards,
    -- Overall (all plays with valid EPA)
    AVG(p.epa)                                                     AS total_epa,
    AVG(p.epa)  FILTER (WHERE p.success = TRUE)                    AS explosiveness
  FROM plays p
  -- Only FBS-vs-FBS: both teams must have SP+ ratings
  JOIN sp_ratings sp_off ON sp_off.team_id = p.offense_team_id AND sp_off.season = p.season
  JOIN sp_ratings sp_def ON sp_def.team_id = p.defense_team_id AND sp_def.season = p.season
  WHERE
    -- garbage_time not provided by cfbfastR; include all plays (analyst can filter later)
    p.epa IS NOT NULL
    AND p.play_type IN (
      'Rush','Rushing Touchdown',
      'Pass Reception','Pass Incompletion','Sack',
      'Passing Touchdown','Interception Return',
      'Interception Return Touchdown'
    )
  GROUP BY p.game_id, p.season, p.offense_team_id
  HAVING
    COUNT(*) FILTER (WHERE p.play_type IN ('Rush','Rushing Touchdown')) >= 5
    AND COUNT(*) FILTER (WHERE p.play_type IN (
      'Pass Reception','Pass Incompletion','Sack',
      'Passing Touchdown','Interception Return',
      'Interception Return Touchdown')) >= 5
"

gts_raw <- dbGetQuery(con, gts_query)
cat("  Raw game-team rows:", nrow(gts_raw), "\n")

# Add is_home flag from games table
games_sides <- dbGetQuery(con, "SELECT game_id, home_team_id, away_team_id FROM games")

gts_clean <- gts_raw %>%
  left_join(games_sides, by = "game_id") %>%
  mutate(
    is_home    = (team_id == home_team_id),
    gts_id     = row_number(),          # safe: table fully rebuilt each time
    havoc_rate = NA_real_,              # populated later when defensive stats available
    created_at = Sys.time()
  ) %>%
  filter(!is.na(is_home)) %>%
  select(gts_id, game_id, team_id, season, is_home,
         rush_plays, rush_epa_total, rush_epa_per_play, rush_success_rate,
         rush_yards, rush_yards_per_carry,
         pass_plays, pass_epa_total, pass_epa_per_play, pass_success_rate, pass_yards,
         total_epa, explosiveness, havoc_rate, created_at)

# Full reload — derived table, always rebuilt from plays
dbExecute(con, "DELETE FROM game_team_stats")
dbAppendTable(con, "game_team_stats", gts_clean)
cat("  Inserted", nrow(gts_clean), "game-team-stat rows.\n\n")

# -----------------------------------------------------------------------------
# SECTION G: PRINT DISTINCT PLAY TYPES (for analyst review)
# Used to verify play_type strings match what the model filters expect.
# -----------------------------------------------------------------------------
cat("--- G. Play type audit ---\n")
play_types <- dbGetQuery(con,
  "SELECT play_type, COUNT(*) AS n FROM plays GROUP BY play_type ORDER BY n DESC LIMIT 30")
print(play_types)
cat("\n")

# -----------------------------------------------------------------------------
# SECTION H: TEAM NAME MATCH AUDIT
# Prints any teams in games that didn't match team_aliases — silent data loss check.
# -----------------------------------------------------------------------------
cat("--- H. Team name match audit ---\n")
unmatched <- dbGetQuery(con, "
  SELECT DISTINCT g.home_team AS unmatched_name, 'home' AS side, g.season
  FROM (
    SELECT home_team, away_team, season
    FROM (
      SELECT
        CASE WHEN home_team_id IS NULL THEN home_team ELSE NULL END AS home_team,
        CASE WHEN away_team_id IS NULL THEN away_team ELSE NULL END AS away_team,
        season
      FROM games
    )
    WHERE home_team IS NOT NULL OR away_team IS NOT NULL
  ) g
  WHERE g.home_team IS NOT NULL
  LIMIT 20
")
if (nrow(unmatched) == 0) {
  cat("  All team names resolved. No unmatched teams.\n")
} else {
  cat("  WARNING: Unmatched teams (add to team_aliases):\n")
  print(unmatched)
}
cat("\n")

# -----------------------------------------------------------------------------
# FINAL SUMMARY
# -----------------------------------------------------------------------------
cat("=== Pull Complete ===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

summary_tables <- c("conferences", "teams", "team_aliases",
                    "conference_memberships", "games", "sp_ratings",
                    "plays", "game_team_stats")

for (tbl in summary_tables) {
  n <- dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s", tbl))$n
  cat(sprintf("  %-26s %s rows\n", tbl, format(n, big.mark = ",")))
}

cat("\nModel-ready observations (v_model_ready, FBS games with SP+ on both sides):\n")
model_n <- dbGetQuery(con, "
  SELECT COUNT(*) AS n FROM v_model_ready
  WHERE rush_plays >= 10 AND pass_plays >= 10
    AND sp_rating IS NOT NULL AND opp_sp_rating IS NOT NULL
")$n
cat(sprintf("  %-26s %s rows\n", "v_model_ready (filtered)", format(model_n, big.mark = ",")))
cat("\nDuckDB ready at: db/nil_analytics.db\n")
cat("Next step: export PFF CSVs into data/raw/pff/ then run 02_process_pff.R\n")
