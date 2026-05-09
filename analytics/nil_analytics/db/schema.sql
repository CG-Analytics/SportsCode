-- =============================================================================
-- NIL Analytics Platform — DuckDB Schema
-- =============================================================================
-- Entity hierarchy:
--   conferences / conference_memberships → realignment-aware conference tracking
--   teams / team_aliases                 → canonical team identity + multi-source name map
--   players / player_team_seasons        → canonical player identity + transfer/redshirt tracking
--   games                                → game metadata (anchors all stats)
--   plays                                → play-by-play (~1M rows for 3 FBS seasons)
--   game_team_stats                      → pre-aggregated EPA/box stats per team-game
--   player_season_grades                 → PFF snap-weighted grades per player-team-season
--   game_player_stats                    → individual game-level stats (future PFF game data)
--   sp_ratings                           → SP+ team ratings per season
-- =============================================================================


-- -----------------------------------------------------------------------------
-- 1. CONFERENCES
-- Canonical conference record — one row per conference identity.
-- Subdivision: 'FBS', 'FCS', 'D2', 'D3', 'NAIA'
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS conferences (
    conference_id   INTEGER PRIMARY KEY,
    name            VARCHAR NOT NULL,          -- e.g. "Big Ten", "SEC", "Sun Belt"
    abbreviation    VARCHAR(15),               -- e.g. "B1G", "SEC", "SBC"
    subdivision     VARCHAR(10) NOT NULL,      -- 'FBS', 'FCS', 'D2', etc.
    UNIQUE (name)
);


-- -----------------------------------------------------------------------------
-- 2. TEAMS
-- Canonical team record. One row per program — no conference or division stored
-- here (that lives in conference_memberships for full realignment history).
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS teams (
    team_id         INTEGER PRIMARY KEY,
    school          VARCHAR NOT NULL,          -- canonical name (e.g. "Ohio State")
    mascot          VARCHAR,                   -- e.g. "Buckeyes"
    abbreviation    VARCHAR(10),               -- e.g. "OSU"
    created_at      TIMESTAMP DEFAULT current_timestamp,
    UNIQUE (school)
);


-- -----------------------------------------------------------------------------
-- 3. CONFERENCE MEMBERSHIPS
-- Tracks which conference (and division within it) a team belongs to,
-- by season range. Handles realignment and FCS→FBS promotions cleanly.
-- A new row is added when a team changes conference or division.
-- season_to = NULL means the membership is currently active.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS conference_memberships (
    membership_id   INTEGER PRIMARY KEY,
    team_id         INTEGER NOT NULL REFERENCES teams(team_id),
    conference_id   INTEGER NOT NULL REFERENCES conferences(conference_id),
    division        VARCHAR(30),               -- e.g. "East", "West", NULL if no divisions
    season_from     INTEGER NOT NULL,          -- first season under this membership
    season_to       INTEGER,                   -- last season (NULL = still active)
    UNIQUE (team_id, season_from)              -- one conference per team per starting season
);

CREATE INDEX IF NOT EXISTS idx_cm_team    ON conference_memberships(team_id);
CREATE INDEX IF NOT EXISTS idx_cm_season  ON conference_memberships(conference_id, season_from);


-- -----------------------------------------------------------------------------
-- 4. TEAM ALIASES
-- Maps external source names → canonical team_id.
-- Solves PFF "Ohio St" vs CFBD "Ohio State" vs ESPN "Ohio St." mismatches.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS team_aliases (
    alias_id        INTEGER PRIMARY KEY,
    team_id         INTEGER NOT NULL REFERENCES teams(team_id),
    source          VARCHAR NOT NULL,          -- 'cfbd', 'pff', 'espn', 'rivals', etc.
    alias_name      VARCHAR NOT NULL,          -- exactly as it appears in that source
    UNIQUE (source, alias_name)
);

-- Index on alias_name for fast lookup when normalizing incoming data
CREATE INDEX IF NOT EXISTS idx_team_aliases_name ON team_aliases(alias_name);


-- -----------------------------------------------------------------------------
-- 5. PLAYERS
-- Canonical player record. Stable across transfers and redshirt years.
-- player_hash is SHA-256 of (last_name || first_name || date_of_birth || high_school)
-- used for cross-source deduplication — NOT the PK (hashes break if source
-- data is later corrected; the integer PK never changes).
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS players (
    player_id       INTEGER PRIMARY KEY,
    player_hash     VARCHAR(64) UNIQUE NOT NULL,
    first_name      VARCHAR NOT NULL,
    last_name       VARCHAR NOT NULL,
    full_name       VARCHAR,                   -- denormalized convenience column
    date_of_birth   DATE,
    high_school     VARCHAR,
    hometown        VARCHAR,
    home_state      VARCHAR(2),
    position        VARCHAR(10),               -- primary career position (e.g. "OT", "DE", "LB")
    created_at      TIMESTAMP DEFAULT current_timestamp
);

-- DOB included to disambiguate the many players who share first+last name
CREATE INDEX IF NOT EXISTS idx_players_name_dob ON players(last_name, first_name, date_of_birth);
CREATE INDEX IF NOT EXISTS idx_players_hash     ON players(player_hash);


-- -----------------------------------------------------------------------------
-- 6. PLAYER-TEAM-SEASONS (Transfer + Redshirt Bridge)
-- One row per player per team per season — the join anchor for all player stats.
--
-- Redshirt handling: a player who redshirts simply has is_redshirt_season=TRUE
-- for that row. Their class_year stays the same the following season (e.g.,
-- a redshirt freshman appears as 'FR' in two consecutive seasons — same
-- class_year, different season values, is_redshirt_season marks which is which).
-- This avoids any ambiguity without adding extra complexity.
--
-- Transfer handling: a player who transfers appears with a new team_id and new
-- season — no special handling needed.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS player_team_seasons (
    pts_id              INTEGER PRIMARY KEY,
    player_id           INTEGER NOT NULL REFERENCES players(player_id),
    team_id             INTEGER NOT NULL REFERENCES teams(team_id),
    season              INTEGER NOT NULL,
    jersey_number       VARCHAR(3),
    position            VARCHAR(10),           -- position that specific season
    class_year          VARCHAR(5),            -- 'FR', 'SO', 'JR', 'SR', 'GR' (graduate)
    is_redshirt_season  BOOLEAN DEFAULT FALSE, -- TRUE = redshirt year (no game eligibility used)
    is_starter          BOOLEAN DEFAULT FALSE,
    UNIQUE (player_id, team_id, season)
);

CREATE INDEX IF NOT EXISTS idx_pts_player ON player_team_seasons(player_id, season);
CREATE INDEX IF NOT EXISTS idx_pts_team   ON player_team_seasons(team_id, season);


-- -----------------------------------------------------------------------------
-- 7. GAMES
-- One row per game. home_team_id and away_team_id are team_id FKs.
-- cfbd_game_id is CFBD's native integer for joining back to raw API data.
-- game_type handles regular season, bowls, conference championships, playoff.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS games (
    game_id         INTEGER PRIMARY KEY,
    cfbd_game_id    INTEGER UNIQUE,
    season          INTEGER NOT NULL,
    week            INTEGER,                   -- regular season week (NULL for postseason)
    game_type       VARCHAR(20),               -- 'regular', 'bowl', 'conference_champ', 'playoff'
    game_date       DATE NOT NULL,
    home_team_id    INTEGER NOT NULL REFERENCES teams(team_id),
    away_team_id    INTEGER NOT NULL REFERENCES teams(team_id),
    home_score      INTEGER,
    away_score      INTEGER,
    neutral_site    BOOLEAN DEFAULT FALSE,
    conference_game BOOLEAN DEFAULT FALSE,
    attendance      INTEGER,
    venue           VARCHAR,
    created_at      TIMESTAMP DEFAULT current_timestamp
);

CREATE INDEX IF NOT EXISTS idx_games_season    ON games(season);
CREATE INDEX IF NOT EXISTS idx_games_home_team ON games(home_team_id, season);
CREATE INDEX IF NOT EXISTS idx_games_away_team ON games(away_team_id, season);


-- -----------------------------------------------------------------------------
-- 8. PLAYS (Play-by-Play)
-- One row per play. Largest table — ~500K–1M rows for 3 FBS seasons.
-- Pulled via cfbfastR by season+week (~45 API calls total for 3 seasons).
-- offense/defense stored as team_id FKs (resolved via team_aliases at load time).
-- This is the source-of-truth table; game_team_stats is derived from it.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS plays (
    play_id             INTEGER PRIMARY KEY,
    cfbd_play_id        BIGINT UNIQUE,
    game_id             INTEGER NOT NULL REFERENCES games(game_id),
    season              INTEGER NOT NULL,
    week                INTEGER,
    offense_team_id     INTEGER NOT NULL REFERENCES teams(team_id),
    defense_team_id     INTEGER NOT NULL REFERENCES teams(team_id),
    play_number         INTEGER,
    period              INTEGER,               -- quarter (1–4, 5+ for OT)
    clock_minutes       INTEGER,
    clock_seconds       INTEGER,
    yard_line           INTEGER,               -- 0–100 normalized field position
    down                INTEGER,
    distance            INTEGER,
    play_type           VARCHAR(50),           -- 'Rush', 'Pass Reception', 'Sack', etc.
    yards_gained        INTEGER,
    scoring             BOOLEAN DEFAULT FALSE,
    score_offense       INTEGER,               -- score at time of play (offense perspective)
    score_defense       INTEGER,
    epa                 DOUBLE,                -- Expected Points Added
    wpa                 DOUBLE,                -- Win Probability Added
    success             BOOLEAN,               -- EPA > 0
    garbage_time        BOOLEAN DEFAULT FALSE, -- flagged for model exclusion
    created_at          TIMESTAMP DEFAULT current_timestamp
);

CREATE INDEX IF NOT EXISTS idx_plays_game    ON plays(game_id);
CREATE INDEX IF NOT EXISTS idx_plays_offense ON plays(offense_team_id, season);
CREATE INDEX IF NOT EXISTS idx_plays_type    ON plays(play_type, season);


-- -----------------------------------------------------------------------------
-- 9. GAME-TEAM STATS
-- Pre-aggregated EPA and box stats at the team-game level (offense perspective).
-- Two rows per game (one per team). Derived from plays with garbage-time filter
-- applied, then stored here so regression scripts don't re-scan 1M play rows.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS game_team_stats (
    gts_id              INTEGER PRIMARY KEY,
    game_id             INTEGER NOT NULL REFERENCES games(game_id),
    team_id             INTEGER NOT NULL REFERENCES teams(team_id),
    season              INTEGER NOT NULL,
    is_home             BOOLEAN,
    -- Rushing
    rush_plays          INTEGER,
    rush_epa_total      DOUBLE,
    rush_epa_per_play   DOUBLE,
    rush_success_rate   DOUBLE,                -- % of rushes with EPA > 0
    rush_yards          INTEGER,
    rush_yards_per_carry DOUBLE,
    -- Passing
    pass_plays          INTEGER,
    pass_epa_total      DOUBLE,
    pass_epa_per_play   DOUBLE,
    pass_success_rate   DOUBLE,
    pass_yards          INTEGER,
    -- Overall
    total_epa           DOUBLE,
    explosiveness       DOUBLE,                -- mean EPA on successful plays only
    havoc_rate          DOUBLE,                -- opponent: (TFL + PD + TO) / plays
    created_at          TIMESTAMP DEFAULT current_timestamp,
    UNIQUE (game_id, team_id)
);

CREATE INDEX IF NOT EXISTS idx_gts_team_season ON game_team_stats(team_id, season);


-- -----------------------------------------------------------------------------
-- 10. SP+ RATINGS
-- Season-level SP+ ratings from CFBD API (cfbd_ratings_sp).
-- One row per team per season.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS sp_ratings (
    sp_id               INTEGER PRIMARY KEY,
    team_id             INTEGER NOT NULL REFERENCES teams(team_id),
    season              INTEGER NOT NULL,
    sp_overall          DOUBLE,
    sp_offense          DOUBLE,
    sp_defense          DOUBLE,
    sp_special_teams    DOUBLE,
    UNIQUE (team_id, season)
);


-- -----------------------------------------------------------------------------
-- 11. PLAYER SEASON GRADES (PFF)
-- Snap-weighted PFF grades per player per team per season.
-- Linked to player_team_seasons via pts_id.
--
-- Position groups:
--   'OL'  — offensive line (run_block_grade, pass_block_grade)
--   'DL'  — defensive line interior (run_stop_grade, pass_rush_grade)
--   'LB'  — linebackers (run_stop_grade, pass_rush_grade, coverage_grade)
--            included because LBs are critical to run defense and hybrid pass rush
--   'RB'  — running backs (rushing_grade, receiving_grade)
--   'QB'  — quarterbacks (passing_grade, overall_grade)
--   'WR'  — wide receivers (route_grade, receiving_grade)
--   'TE'  — tight ends (route_grade, receiving_grade, run_block_grade)
--   'DB'  — defensive backs (coverage_grade, tackling_grade)
--
-- Grade columns are NULL when they don't apply to the position group.
-- Snap-weighting and top-N filtering are applied at load time (see 02_process_pff.R).
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS player_season_grades (
    psg_id              INTEGER PRIMARY KEY,
    pts_id              INTEGER NOT NULL REFERENCES player_team_seasons(pts_id),
    player_id           INTEGER NOT NULL REFERENCES players(player_id),
    team_id             INTEGER NOT NULL REFERENCES teams(team_id),
    season              INTEGER NOT NULL,
    position_group      VARCHAR(5) NOT NULL,   -- 'OL', 'DL', 'LB', 'RB', 'QB', 'WR', 'TE', 'DB'
    snap_count          INTEGER,
    -- Shared / overall
    overall_grade       DOUBLE,                -- PFF overall grade (0–100)
    tackling_grade      DOUBLE,                -- DL, LB, DB
    -- OL grades
    run_block_grade     DOUBLE,                -- OL, TE
    pass_block_grade    DOUBLE,                -- OL
    -- DL / LB grades (defensive front 7)
    run_stop_grade      DOUBLE,                -- DL, LB
    pass_rush_grade     DOUBLE,                -- DL, LB (edge/hybrid rushers)
    -- LB-specific
    coverage_grade      DOUBLE,                -- LB, DB
    -- RB grades
    rushing_grade       DOUBLE,                -- RB
    receiving_grade     DOUBLE,                -- RB, WR, TE
    -- QB grades
    passing_grade       DOUBLE,                -- QB
    -- WR / TE grades
    route_grade         DOUBLE,                -- WR, TE
    UNIQUE (player_id, team_id, season, position_group)
);

CREATE INDEX IF NOT EXISTS idx_psg_team_season    ON player_season_grades(team_id, season, position_group);
CREATE INDEX IF NOT EXISTS idx_psg_player_season  ON player_season_grades(player_id, season);


-- -----------------------------------------------------------------------------
-- 12. GAME PLAYER STATS (Future — game-level PFF data)
-- Individual player stats at the game level.
-- Kept sparse now; populate when game-level PFF data becomes available.
-- Same position_group logic as player_season_grades.
-- -----------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS game_player_stats (
    gps_id              INTEGER PRIMARY KEY,
    player_id           INTEGER NOT NULL REFERENCES players(player_id),
    game_id             INTEGER NOT NULL REFERENCES games(game_id),
    team_id             INTEGER NOT NULL REFERENCES teams(team_id),
    season              INTEGER NOT NULL,
    position_group      VARCHAR(5),
    snap_count          INTEGER,
    overall_grade       DOUBLE,
    run_block_grade     DOUBLE,
    pass_block_grade    DOUBLE,
    run_stop_grade      DOUBLE,
    pass_rush_grade     DOUBLE,
    coverage_grade      DOUBLE,
    rushing_grade       DOUBLE,
    passing_grade       DOUBLE,
    UNIQUE (player_id, game_id)
);

CREATE INDEX IF NOT EXISTS idx_gps_game   ON game_player_stats(game_id);
CREATE INDEX IF NOT EXISTS idx_gps_player ON game_player_stats(player_id, season);


-- =============================================================================
-- CONVENIENCE VIEWS
-- =============================================================================

-- Helper: resolve a team's conference for a given season
CREATE VIEW IF NOT EXISTS v_team_conference AS
SELECT
    t.team_id,
    t.school,
    t.abbreviation,
    c.name          AS conference,
    c.abbreviation  AS conf_abbrev,
    c.subdivision,
    cm.division,
    cm.season_from,
    cm.season_to
FROM teams t
JOIN conference_memberships cm ON cm.team_id = t.team_id
JOIN conferences c             ON c.conference_id = cm.conference_id;


-- Model-ready view: one row per team-game with all grades, SP+, and conference joined
-- This matches the shape of the final dataframe fed into the regression scripts.
-- Note: player_season_grades rows here are team-season aggregates (pre-computed
-- snap-weighted grades for the top-N players at each position group).
CREATE VIEW IF NOT EXISTS v_model_ready AS
SELECT
    g.game_id,
    g.season,
    g.week,
    g.game_date,
    g.game_type,
    -- Offense team
    gts.team_id,
    t_off.school                            AS team,
    gts.is_home,
    tc_off.conference                       AS team_conference,
    tc_off.subdivision                      AS team_subdivision,
    -- EPA outcomes
    gts.rush_epa_per_play,
    gts.pass_epa_per_play,
    gts.rush_success_rate,
    gts.pass_success_rate,
    gts.rush_plays,
    gts.pass_plays,
    -- SP+ for offense
    sp_off.sp_overall                       AS sp_rating,
    sp_off.sp_offense,
    sp_off.sp_defense,
    -- Opponent
    CASE WHEN gts.is_home
         THEN g.away_team_id
         ELSE g.home_team_id END            AS opp_team_id,
    t_def.school                            AS opponent,
    tc_def.conference                       AS opp_conference,
    -- SP+ for opponent
    sp_def.sp_overall                       AS opp_sp_rating,
    sp_def.sp_offense                       AS opp_sp_offense,
    sp_def.sp_defense                       AS opp_sp_defense,
    -- OL grades (snap-weighted top 5, pre-aggregated)
    ol.run_block_grade                      AS ol_run_grade,
    ol.pass_block_grade                     AS ol_pass_grade,
    -- DL grades — opponent's defensive line
    dl.run_stop_grade                       AS dl_run_grade,
    dl.pass_rush_grade                      AS dl_pass_grade,
    -- LB grades — opponent's linebackers (run stop + pass rush)
    lb.run_stop_grade                       AS lb_run_grade,
    lb.pass_rush_grade                      AS lb_pass_rush_grade,
    lb.coverage_grade                       AS lb_coverage_grade,
    -- RB and QB control grades
    rb.rushing_grade                        AS rb_grade,
    qb.passing_grade                        AS qb_grade
FROM games g
JOIN game_team_stats gts
    ON gts.game_id = g.game_id
JOIN teams t_off
    ON t_off.team_id = gts.team_id
-- Resolve opponent
JOIN teams t_def
    ON t_def.team_id = CASE WHEN gts.is_home THEN g.away_team_id ELSE g.home_team_id END
-- Conference for offense team (for the game's season)
LEFT JOIN v_team_conference tc_off
    ON tc_off.team_id = gts.team_id
    AND g.season >= tc_off.season_from
    AND (tc_off.season_to IS NULL OR g.season <= tc_off.season_to)
-- Conference for opponent (for the game's season)
LEFT JOIN v_team_conference tc_def
    ON tc_def.team_id = t_def.team_id
    AND g.season >= tc_def.season_from
    AND (tc_def.season_to IS NULL OR g.season <= tc_def.season_to)
-- SP+ — offense
LEFT JOIN sp_ratings sp_off
    ON sp_off.team_id = gts.team_id AND sp_off.season = g.season
-- SP+ — opponent
LEFT JOIN sp_ratings sp_def
    ON sp_def.team_id = t_def.team_id AND sp_def.season = g.season
-- OL grades — offense
LEFT JOIN player_season_grades ol
    ON ol.team_id = gts.team_id AND ol.season = g.season AND ol.position_group = 'OL'
-- DL grades — opponent
LEFT JOIN player_season_grades dl
    ON dl.team_id = t_def.team_id AND dl.season = g.season AND dl.position_group = 'DL'
-- LB grades — opponent
LEFT JOIN player_season_grades lb
    ON lb.team_id = t_def.team_id AND lb.season = g.season AND lb.position_group = 'LB'
-- RB grades — offense
LEFT JOIN player_season_grades rb
    ON rb.team_id = gts.team_id AND rb.season = g.season AND rb.position_group = 'RB'
-- QB grades — offense
LEFT JOIN player_season_grades qb
    ON qb.team_id = gts.team_id AND qb.season = g.season AND qb.position_group = 'QB';
