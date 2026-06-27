-- =============================================================================
-- College Basketball Analytics Database
-- PostgreSQL Schema
-- =============================================================================
-- Tables:
--   teams            - ESPN team metadata (stable reference)
--   games            - One row per game (metadata, venue, status)
--   team_box         - One row per team per game (box score + advanced metrics)
--   player_box       - One row per player per game
--   betting_lines    - One row per provider per game (spread, ML, totals)
--   win_probability  - One row per play per game (home win %)
--   plays            - Full play-by-play with shot coordinates
--   on_off           - Player rotation stints with plus/minus
--   leaders          - Stat leaders per category per game
--   torvik_ratings   - Daily Bart Torvik team ratings snapshot
-- =============================================================================

-- ---------------------------------------------------------------------------
-- EXTENSIONS
-- ---------------------------------------------------------------------------
CREATE EXTENSION IF NOT EXISTS "pgcrypto";  -- for gen_random_uuid() if needed


-- ---------------------------------------------------------------------------
-- TEAMS
-- Reference table. Populated on first encounter; updated as new names appear.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS teams (
    team_id         TEXT        PRIMARY KEY,  -- ESPN team id (string)
    display_name    TEXT,
    short_name      TEXT,
    abbreviation    TEXT,
    conference      TEXT,
    color           TEXT,                     -- ESPN brand hex color
    logo_url        TEXT,
    created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_teams_abbreviation ON teams (abbreviation);


-- ---------------------------------------------------------------------------
-- GAMES
-- One row per game. Core metadata drawn from ESPN header + game_info.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS games (
    game_id             TEXT        PRIMARY KEY,  -- ESPN event id
    season              TEXT        NOT NULL,     -- e.g. "2024-25"
    game_date           DATE        NOT NULL,
    game_datetime       TIMESTAMPTZ,              -- tip-off time w/ tz if available
    home_team_id        TEXT        REFERENCES teams (team_id),
    away_team_id        TEXT        REFERENCES teams (team_id),
    home_score          INTEGER,
    away_score          INTEGER,
    status              TEXT,                     -- "final", "in_progress", "scheduled"
    period              INTEGER,                  -- periods played (2 regulation, 3+ OT)
    neutral_site        BOOLEAN     DEFAULT FALSE,
    conference_game     BOOLEAN,
    venue_id            TEXT,
    venue_name          TEXT,
    attendance          INTEGER,
    officials           TEXT,                     -- semicolon-separated names
    espn_group          TEXT,                     -- ESPN group/division filter used
    created_at          TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at          TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_games_season       ON games (season);
CREATE INDEX IF NOT EXISTS idx_games_game_date    ON games (game_date);
CREATE INDEX IF NOT EXISTS idx_games_home_team    ON games (home_team_id);
CREATE INDEX IF NOT EXISTS idx_games_away_team    ON games (away_team_id);


-- ---------------------------------------------------------------------------
-- TEAM_BOX
-- One row per team per game. Box score + derived advanced metrics.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS team_box (
    id                              BIGSERIAL   PRIMARY KEY,
    game_id                         TEXT        NOT NULL REFERENCES games (game_id) ON DELETE CASCADE,
    team_id                         TEXT        NOT NULL REFERENCES teams (team_id),
    season                          TEXT        NOT NULL,
    game_date                       DATE        NOT NULL,
    home_away                       TEXT,                   -- "home" | "away"
    opponent_team_id                TEXT        REFERENCES teams (team_id),

    -- Raw box score
    points                          NUMERIC,
    points_allowed                  NUMERIC,
    margin                          NUMERIC,
    won                             BOOLEAN,

    field_goals_made                NUMERIC,
    field_goals_attempted           NUMERIC,
    three_point_field_goals_made    NUMERIC,
    three_point_field_goals_attempted NUMERIC,
    two_point_field_goals_made      NUMERIC,
    two_point_field_goals_attempted NUMERIC,
    free_throws_made                NUMERIC,
    free_throws_attempted           NUMERIC,
    rebounds                        NUMERIC,
    offensive_rebounds              NUMERIC,
    defensive_rebounds              NUMERIC,
    assists                         NUMERIC,
    turnovers                       NUMERIC,
    steals                          NUMERIC,
    blocks                          NUMERIC,
    fouls                           NUMERIC,

    -- Advanced / derived metrics
    fg_pct                          NUMERIC,
    threep_pct                      NUMERIC,
    ft_pct                          NUMERIC,
    effective_field_goal_pct        NUMERIC,    -- eFG%
    true_shooting_pct               NUMERIC,    -- TS%
    free_throw_rate                 NUMERIC,    -- FTA/FGA
    three_point_attempt_rate        NUMERIC,    -- 3PA/FGA
    assist_to_turnover_ratio        NUMERIC,
    estimated_possessions           NUMERIC,
    points_per_estimated_possession NUMERIC,
    eff                             NUMERIC,    -- pts per 100 possessions
    pace                            NUMERIC,
    ftar                            NUMERIC,    -- FTA per 100 poss
    fgar                            NUMERIC,    -- FGA per 100 poss
    threepar                        NUMERIC,    -- 3PA per 100 poss
    pct_3pa                         NUMERIC,

    created_at                      TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    UNIQUE (game_id, team_id)
);

CREATE INDEX IF NOT EXISTS idx_team_box_game_id   ON team_box (game_id);
CREATE INDEX IF NOT EXISTS idx_team_box_team_id   ON team_box (team_id);
CREATE INDEX IF NOT EXISTS idx_team_box_season     ON team_box (season);
CREATE INDEX IF NOT EXISTS idx_team_box_game_date  ON team_box (game_date);


-- ---------------------------------------------------------------------------
-- PLAYER_BOX
-- One row per player per game.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS player_box (
    id                              BIGSERIAL   PRIMARY KEY,
    game_id                         TEXT        NOT NULL REFERENCES games (game_id) ON DELETE CASCADE,
    team_id                         TEXT        NOT NULL REFERENCES teams (team_id),
    season                          TEXT        NOT NULL,
    game_date                       DATE        NOT NULL,
    player_id                       TEXT        NOT NULL,
    display_name                    TEXT,
    jersey                          TEXT,
    position                        TEXT,
    starter                         BOOLEAN,
    did_not_play                    BOOLEAN,

    -- Raw box score
    minutes_numeric                 NUMERIC,
    points                          NUMERIC,
    field_goals_made                NUMERIC,
    field_goals_attempted           NUMERIC,
    three_point_field_goals_made    NUMERIC,
    three_point_field_goals_attempted NUMERIC,
    free_throws_made                NUMERIC,
    free_throws_attempted           NUMERIC,
    rebounds                        NUMERIC,
    offensive_rebounds              NUMERIC,
    defensive_rebounds              NUMERIC,
    assists                         NUMERIC,
    turnovers                       NUMERIC,
    steals                          NUMERIC,
    blocks                          NUMERIC,
    fouls                           NUMERIC,
    plus_minus                      NUMERIC,

    -- Advanced
    fg_pct                          NUMERIC,
    threep_pct                      NUMERIC,
    ft_pct                          NUMERIC,
    effective_field_goal_pct        NUMERIC,
    true_shooting_pct               NUMERIC,
    estimated_possessions           NUMERIC,
    points_per_estimated_possession NUMERIC,
    points_per_minute               NUMERIC,

    created_at                      TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    UNIQUE (game_id, team_id, player_id)
);

CREATE INDEX IF NOT EXISTS idx_player_box_game_id   ON player_box (game_id);
CREATE INDEX IF NOT EXISTS idx_player_box_team_id   ON player_box (team_id);
CREATE INDEX IF NOT EXISTS idx_player_box_player_id ON player_box (player_id);
CREATE INDEX IF NOT EXISTS idx_player_box_season     ON player_box (season);


-- ---------------------------------------------------------------------------
-- BETTING_LINES
-- One row per provider per game. Sourced from ESPN pickcenter.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS betting_lines (
    id                      BIGSERIAL   PRIMARY KEY,
    game_id                 TEXT        NOT NULL REFERENCES games (game_id) ON DELETE CASCADE,
    provider_id             TEXT,
    provider_name           TEXT,

    -- Spread
    spread                  NUMERIC,
    details                 TEXT,               -- e.g. "Duke -5.5"
    home_spread_open        NUMERIC,
    home_spread_close       NUMERIC,
    away_spread_open        NUMERIC,
    away_spread_close       NUMERIC,
    home_spread_odds        NUMERIC,            -- juice on spread (home)
    away_spread_odds        NUMERIC,            -- juice on spread (away)

    -- Moneyline
    home_moneyline          NUMERIC,
    away_moneyline          NUMERIC,
    home_moneyline_open     NUMERIC,
    home_moneyline_close    NUMERIC,
    away_moneyline_open     NUMERIC,
    away_moneyline_close    NUMERIC,
    home_favorite           BOOLEAN,
    away_favorite           BOOLEAN,
    home_favorite_open      BOOLEAN,
    away_favorite_open      BOOLEAN,

    -- Totals (over/under)
    over_under              NUMERIC,
    over_odds               NUMERIC,
    under_odds              NUMERIC,
    total_over_open         NUMERIC,
    total_over_close        NUMERIC,
    total_under_open        NUMERIC,
    total_under_close       NUMERIC,

    -- Foreign keys to teams for convenience
    home_team_id            TEXT        REFERENCES teams (team_id),
    away_team_id            TEXT        REFERENCES teams (team_id),

    created_at              TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    UNIQUE (game_id, provider_id)
);

CREATE INDEX IF NOT EXISTS idx_betting_lines_game_id ON betting_lines (game_id);


-- ---------------------------------------------------------------------------
-- WIN_PROBABILITY
-- One row per play per game. Home win % at each play.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS win_probability (
    id                      BIGSERIAL   PRIMARY KEY,
    game_id                 TEXT        NOT NULL REFERENCES games (game_id) ON DELETE CASCADE,
    play_id                 TEXT,
    home_team_id            TEXT        REFERENCES teams (team_id),
    away_team_id            TEXT        REFERENCES teams (team_id),
    home_win_percentage     NUMERIC,
    tie_percentage          NUMERIC,

    created_at              TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_win_prob_game_id ON win_probability (game_id);
CREATE INDEX IF NOT EXISTS idx_win_prob_play_id ON win_probability (play_id);


-- ---------------------------------------------------------------------------
-- PLAYS
-- Full play-by-play. One row per play event.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS plays (
    id                  BIGSERIAL   PRIMARY KEY,
    game_id             TEXT        NOT NULL REFERENCES games (game_id) ON DELETE CASCADE,
    play_id             TEXT,
    sequence_number     TEXT,
    play_type_id        TEXT,
    play_type           TEXT,
    text                TEXT,
    short_description   TEXT,
    team_id             TEXT        REFERENCES teams (team_id),
    period              INTEGER,
    period_display      TEXT,
    clock               TEXT,               -- "MM:SS" remaining
    game_seconds        NUMERIC,            -- elapsed seconds from tip
    away_score          INTEGER,
    home_score          INTEGER,
    scoring_play        BOOLEAN,
    score_value         NUMERIC,
    shooting_play       BOOLEAN,
    x_coordinate        NUMERIC,
    y_coordinate        NUMERIC,
    shot_distance       NUMERIC,            -- feet from basket
    points_attempted    NUMERIC,            -- 2 or 3
    wall_clock          TEXT,
    participant1_id     TEXT,
    participant2_id     TEXT,

    created_at          TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_plays_game_id      ON plays (game_id);
CREATE INDEX IF NOT EXISTS idx_plays_team_id      ON plays (team_id);
CREATE INDEX IF NOT EXISTS idx_plays_shooting     ON plays (shooting_play) WHERE shooting_play = TRUE;
CREATE INDEX IF NOT EXISTS idx_plays_scoring      ON plays (scoring_play)  WHERE scoring_play  = TRUE;


-- ---------------------------------------------------------------------------
-- ON_OFF
-- Player rotation stints derived from substitution events.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS on_off (
    id              BIGSERIAL   PRIMARY KEY,
    game_id         TEXT        NOT NULL REFERENCES games (game_id) ON DELETE CASCADE,
    team_id         TEXT        NOT NULL REFERENCES teams (team_id),
    player_id       TEXT        NOT NULL,
    start_time      NUMERIC,    -- game seconds elapsed at stint start
    end_time        NUMERIC,    -- game seconds elapsed at stint end
    minutes         NUMERIC,
    plus_minus      NUMERIC,

    created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_on_off_game_id   ON on_off (game_id);
CREATE INDEX IF NOT EXISTS idx_on_off_player_id ON on_off (player_id);
CREATE INDEX IF NOT EXISTS idx_on_off_team_id   ON on_off (team_id);


-- ---------------------------------------------------------------------------
-- LEADERS
-- Stat leaders per category per game (points, rebounds, assists, etc.)
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS leaders (
    id                      BIGSERIAL   PRIMARY KEY,
    game_id                 TEXT        NOT NULL REFERENCES games (game_id) ON DELETE CASCADE,
    team_id                 TEXT        REFERENCES teams (team_id),
    team_display_name       TEXT,
    category                TEXT,           -- e.g. "points", "rebounds"
    category_display        TEXT,
    player_id               TEXT,
    player_display_name     TEXT,
    display_value           TEXT,
    main_stat_value         TEXT,
    main_stat_label         TEXT,

    created_at              TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_leaders_game_id ON leaders (game_id);


-- ---------------------------------------------------------------------------
-- TORVIK_RATINGS
-- Daily Bart Torvik team ratings snapshots (from timeMachine).
-- One row per team per snapshot date.
-- ---------------------------------------------------------------------------
CREATE TABLE IF NOT EXISTS torvik_ratings (
    id                  BIGSERIAL   PRIMARY KEY,
    snapshot_date       DATE        NOT NULL,
    team                TEXT        NOT NULL,
    conference          TEXT,
    record              TEXT,

    -- Core ratings
    adj_oe              NUMERIC,    -- adjusted offensive efficiency
    adj_de              NUMERIC,    -- adjusted defensive efficiency
    barthag             NUMERIC,    -- power rating (win prob vs avg team)
    adj_tempo           NUMERIC,

    -- Rankings
    rank                INTEGER,
    oe_rank             INTEGER,
    de_rank             INTEGER,

    -- Projected record
    proj_w              NUMERIC,
    proj_l              NUMERIC,

    -- Strength of schedule
    sos                 NUMERIC,
    ncsos               NUMERIC,
    consos              NUMERIC,
    elite_sos           NUMERIC,

    -- Quality metrics
    qual_o              NUMERIC,
    qual_d              NUMERIC,
    qual_barthag        NUMERIC,
    qual_games          NUMERIC,

    -- Conference stats
    con_oe              NUMERIC,
    con_de              NUMERIC,
    conf_win_pct        NUMERIC,

    -- WAB (wins above bubble)
    wab                 NUMERIC,
    wab_rank            INTEGER,

    -- Fun rating
    fun                 NUMERIC,
    fun_rank            INTEGER,

    created_at          TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    UNIQUE (snapshot_date, team)
);

CREATE INDEX IF NOT EXISTS idx_torvik_date ON torvik_ratings (snapshot_date);
CREATE INDEX IF NOT EXISTS idx_torvik_team ON torvik_ratings (team);


-- ---------------------------------------------------------------------------
-- UPDATED_AT TRIGGER (reusable for tables with updated_at column)
-- ---------------------------------------------------------------------------
CREATE OR REPLACE FUNCTION set_updated_at()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE TRIGGER trg_teams_updated_at
    BEFORE UPDATE ON teams
    FOR EACH ROW EXECUTE FUNCTION set_updated_at();

CREATE OR REPLACE TRIGGER trg_games_updated_at
    BEFORE UPDATE ON games
    FOR EACH ROW EXECUTE FUNCTION set_updated_at();
