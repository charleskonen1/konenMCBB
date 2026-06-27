# =============================================================================
# load_espn_db_to_postgres.R
#
# Bulk-loads all historical ESPN game data from the local RDS file database
# into PostgreSQL. Safe to re-run — all inserts use ON CONFLICT DO NOTHING
# or upsert logic so existing rows are never duplicated.
#
# Usage:
#   Rscript load_espn_db_to_postgres.R
#
# Prerequisites:
#   install.packages(c("DBI", "RPostgres", "dplyr", "purrr", "tibble",
#                      "stringr", "lubridate", "cli"))
#
# Environment variables (or fill in the config block below):
#   PGHOST, PGPORT, PGDBNAME, PGUSER, PGPASSWORD
# =============================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(stringr)
  library(lubridate)
  library(cli)
})

# -----------------------------------------------------------------------------
# CONFIG — edit here or set environment variables
# -----------------------------------------------------------------------------
pg_config <- list(
  host     = Sys.getenv("PGHOST",     "localhost"),
  port     = as.integer(Sys.getenv("PGPORT", "5432")),
  dbname   = Sys.getenv("PGDBNAME",   "cbb_analytics"),
  user     = Sys.getenv("PGUSER",     "postgres"),
  password = Sys.getenv("PGPASSWORD", "")
)

# Path to local ESPN RDS database (override with env var or edit directly)
ESPN_DB_PATH <- Sys.getenv("ESPN_DB_PATH", "data/espn_db")

# Seasons to load — add more as needed
SEASONS <- c("2024-25")

# Batch size for inserts (rows per transaction)
BATCH_SIZE <- 500L

# -----------------------------------------------------------------------------
# HELPERS
# -----------------------------------------------------------------------------

con_open <- function() {
  dbConnect(
    RPostgres::Postgres(),
    host     = pg_config$host,
    port     = pg_config$port,
    dbname   = pg_config$dbname,
    user     = pg_config$user,
    password = pg_config$password
  )
}

# Safe numeric coercion
as_num <- function(x) suppressWarnings(as.numeric(x))

# Safe logical coercion
as_lgl <- function(x) {
  if (is.logical(x)) return(x)
  x <- tolower(as.character(x))
  case_when(x %in% c("true", "1", "yes") ~ TRUE,
            x %in% c("false", "0", "no") ~ FALSE,
            TRUE ~ NA)
}

# Upsert helper: INSERT ... ON CONFLICT DO NOTHING
# For tables with a known unique key use dbWriteTable or manual SQL.
upsert_do_nothing <- function(con, table, df, conflict_cols) {
  if (nrow(df) == 0L) return(invisible(0L))

  # Write to a temp table then insert-select with ON CONFLICT
  tmp <- paste0("_tmp_", table, "_", as.integer(Sys.time()))
  dbWriteTable(con, tmp, df, temporary = TRUE, overwrite = TRUE)

  cols     <- paste(DBI::dbQuoteIdentifier(con, names(df)), collapse = ", ")
  conflict <- paste(DBI::dbQuoteIdentifier(con, conflict_cols), collapse = ", ")

  sql <- glue::glue_sql(
    "INSERT INTO {`table`} ({cols})
     SELECT {cols} FROM {`tmp`}
     ON CONFLICT ({conflict}) DO NOTHING",
    .con = con
  )
  rows <- dbExecute(con, sql)
  dbExecute(con, paste0("DROP TABLE IF EXISTS ", tmp))
  invisible(rows)
}

# Batch insert with ON CONFLICT DO NOTHING (no glue dependency version)
batch_insert <- function(con, table, df, conflict_cols) {
  if (nrow(df) == 0L) return(invisible(0L))
  total <- 0L
  batches <- split(df, ceiling(seq_len(nrow(df)) / BATCH_SIZE))
  for (batch in batches) {
    tmp <- paste0("tmp_load_", gsub("[^a-z0-9]", "_", table))
    dbWriteTable(con, tmp, batch, temporary = TRUE, overwrite = TRUE)
    cols     <- paste(dbQuoteIdentifier(con, names(batch)), collapse = ", ")
    conflict <- paste(dbQuoteIdentifier(con, conflict_cols), collapse = ", ")
    sql <- sprintf(
      "INSERT INTO %s (%s) SELECT %s FROM %s ON CONFLICT (%s) DO NOTHING",
      dbQuoteIdentifier(con, table), cols, cols,
      dbQuoteIdentifier(con, tmp),   conflict
    )
    total <- total + dbExecute(con, sql)
    dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", dbQuoteIdentifier(con, tmp)))
  }
  invisible(total)
}

# List all game directories for a season
list_game_dirs <- function(base_path, season) {
  season_dir <- file.path(base_path, season)
  if (!dir.exists(season_dir)) return(character(0L))
  day_dirs  <- list.dirs(season_dir, full.names = TRUE, recursive = FALSE)
  game_dirs <- unlist(lapply(day_dirs, function(d) {
    list.dirs(d, full.names = TRUE, recursive = FALSE)
  }), use.names = FALSE)
  game_dirs[dir.exists(game_dirs)]
}

# Parse path parts: returns list(season, date, game_id)
parse_game_path <- function(game_dir) {
  parts <- str_split(normalizePath(game_dir, mustWork = FALSE), .Platform$file.sep)[[1L]]
  n <- length(parts)
  list(
    game_id  = parts[n],
    date_chr = parts[n - 1L],
    season   = parts[n - 2L]
  )
}

safe_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

# -----------------------------------------------------------------------------
# PER-TABLE EXTRACTION FUNCTIONS
# Each takes a game_dir + path info and returns a clean tibble (or NULL).
# -----------------------------------------------------------------------------

extract_game_info <- function(game_dir, meta) {
  game_obj  <- safe_read(file.path(game_dir, "game.rds"))
  game_info <- safe_read(file.path(game_dir, "game_info.rds"))
  if (is.null(game_obj)) return(NULL)

  # Pull header competitors
  comps       <- game_obj$header$competitions
  competitors <- if (!is.null(comps) && length(comps) > 0L) comps[[1]]$competitors else list()

  home_team_id  <- NA_character_
  away_team_id  <- NA_character_
  home_score    <- NA_integer_
  away_score    <- NA_integer_
  neutral_site  <- FALSE
  conf_game     <- NA
  status_text   <- NA_character_
  period        <- NA_integer_
  game_datetime <- NA_character_

  for (cmp in competitors) {
    ha  <- cmp$homeAway
    tid <- as.character(cmp$team$id)
    sc  <- suppressWarnings(as.integer(cmp$score))
    if (identical(ha, "home")) { home_team_id <- tid; home_score <- sc }
    if (identical(ha, "away")) { away_team_id <- tid; away_score <- sc }
  }

  if (!is.null(comps) && length(comps) > 0L) {
    comp1        <- comps[[1]]
    neutral_site <- isTRUE(comp1$neutralSite)
    conf_game    <- isTRUE(comp1$conferenceCompetition)
    period       <- suppressWarnings(as.integer(comp1$status$period))
    status_text  <- tolower(as.character(
      comp1$status$type$description %||% comp1$status$type$name %||% NA_character_
    ))
    dt_raw <- comp1$date
    if (!is.null(dt_raw)) game_datetime <- as.character(dt_raw)
  }

  venue_id   <- NA_character_
  venue_name <- NA_character_
  attendance <- NA_integer_
  officials  <- NA_character_

  if (!is.null(game_info) && nrow(game_info) > 0L) {
    venue_id   <- as.character(game_info$venue_id[1L]   %||% NA_character_)
    venue_name <- as.character(game_info$venue_full_name[1L] %||% NA_character_)
    attendance <- suppressWarnings(as.integer(game_info$attendance[1L]))
    officials  <- as.character(game_info$officials[1L]  %||% NA_character_)
  }

  tibble(
    game_id       = meta$game_id,
    season        = meta$season,
    game_date     = as.Date(meta$date_chr),
    game_datetime = game_datetime,
    home_team_id  = home_team_id,
    away_team_id  = away_team_id,
    home_score    = home_score,
    away_score    = away_score,
    status        = status_text,
    period        = period,
    neutral_site  = neutral_site,
    conference_game = as.logical(conf_game),
    venue_id      = venue_id,
    venue_name    = venue_name,
    attendance    = attendance,
    officials     = officials,
    espn_group    = NA_character_
  )
}

extract_teams <- function(game_dir) {
  game_obj <- safe_read(file.path(game_dir, "game.rds"))
  if (is.null(game_obj)) return(NULL)

  comps       <- game_obj$header$competitions
  competitors <- if (!is.null(comps) && length(comps) > 0L) comps[[1]]$competitors else list()

  rows <- map_dfr(competitors, function(cmp) {
    team <- cmp$team
    if (is.null(team)) return(NULL)
    tibble(
      team_id      = as.character(team$id    %||% NA_character_),
      display_name = as.character(team$displayName %||% NA_character_),
      short_name   = as.character(team$shortDisplayName %||% NA_character_),
      abbreviation = as.character(team$abbreviation %||% NA_character_),
      conference   = NA_character_,
      color        = as.character(team$color %||% NA_character_),
      logo_url     = NA_character_
    )
  })
  rows
}

extract_team_box <- function(game_dir, meta) {
  df <- safe_read(file.path(game_dir, "box_team_stats.rds"))
  if (is.null(df) || nrow(df) == 0L) return(NULL)

  df <- as_tibble(df)
  df$game_id   <- meta$game_id
  df$season    <- meta$season
  df$game_date <- as.Date(meta$date_chr)

  # Opponent id
  if ("team_id" %in% names(df) && nrow(df) == 2L) {
    df$opponent_team_id <- rev(df$team_id)
  } else {
    df$opponent_team_id <- NA_character_
  }

  # Points allowed + margin + won (may already exist from analytics tables)
  if ("points" %in% names(df) && nrow(df) == 2L) {
    pts <- as_num(df$points)
    df$points_allowed <- rev(pts)
    df$margin         <- pts - df$points_allowed
    df$won            <- !is.na(df$margin) & df$margin > 0
  }

  # Normalise all numeric columns we care about
  num_cols <- c(
    "points", "points_allowed", "margin",
    "field_goals_made", "field_goals_attempted",
    "three_point_field_goals_made", "three_point_field_goals_attempted",
    "two_point_field_goals_made", "two_point_field_goals_attempted",
    "free_throws_made", "free_throws_attempted",
    "rebounds", "offensiveRebounds", "offensive_rebounds",
    "defensiveRebounds", "defensive_rebounds",
    "assists", "turnovers", "steals", "blocks", "fouls",
    "fg_pct", "threep_pct", "ft_pct",
    "effective_field_goal_pct", "true_shooting_pct",
    "free_throw_rate", "three_point_attempt_rate",
    "assist_to_turnover_ratio", "estimated_possessions",
    "points_per_estimated_possession", "eff", "pace",
    "ftar", "fgar", "threepar", "pct_3pa"
  )
  for (nm in intersect(num_cols, names(df))) {
    df[[nm]] <- as_num(df[[nm]])
  }

  # Rename ESPN camelCase columns to snake_case expected by schema
  rename_map <- c(
    offensiveRebounds  = "offensive_rebounds",
    defensiveRebounds  = "defensive_rebounds"
  )
  for (old in names(rename_map)) {
    new <- rename_map[[old]]
    if (old %in% names(df) && !(new %in% names(df))) {
      df[[new]] <- df[[old]]
    }
  }

  # Select only schema columns (extras are silently dropped)
  schema_cols <- c(
    "game_id", "team_id", "season", "game_date", "home_away", "opponent_team_id",
    "points", "points_allowed", "margin", "won",
    "field_goals_made", "field_goals_attempted",
    "three_point_field_goals_made", "three_point_field_goals_attempted",
    "two_point_field_goals_made", "two_point_field_goals_attempted",
    "free_throws_made", "free_throws_attempted",
    "rebounds", "offensive_rebounds", "defensive_rebounds",
    "assists", "turnovers", "steals", "blocks", "fouls",
    "fg_pct", "threep_pct", "ft_pct",
    "effective_field_goal_pct", "true_shooting_pct",
    "free_throw_rate", "three_point_attempt_rate",
    "assist_to_turnover_ratio", "estimated_possessions",
    "points_per_estimated_possession", "eff", "pace",
    "ftar", "fgar", "threepar", "pct_3pa"
  )
  df[, intersect(schema_cols, names(df)), drop = FALSE]
}

extract_player_box <- function(game_dir, meta) {
  df <- safe_read(file.path(game_dir, "box_players.rds"))
  if (is.null(df) || nrow(df) == 0L) return(NULL)

  df <- as_tibble(df)
  df$game_id   <- meta$game_id
  df$season    <- meta$season
  df$game_date <- as.Date(meta$date_chr)

  num_cols <- c(
    "minutes_numeric", "points",
    "field_goals_made", "field_goals_attempted",
    "three_point_field_goals_made", "three_point_field_goals_attempted",
    "free_throws_made", "free_throws_attempted",
    "rebounds", "offensiveRebounds", "offensive_rebounds",
    "defensiveRebounds", "defensive_rebounds",
    "assists", "turnovers", "steals", "blocks", "fouls",
    "plus_minus", "fg_pct", "threep_pct", "ft_pct",
    "effective_field_goal_pct", "true_shooting_pct",
    "estimated_possessions", "points_per_estimated_possession",
    "points_per_minute"
  )
  for (nm in intersect(num_cols, names(df))) df[[nm]] <- as_num(df[[nm]])

  # Rename camelCase
  rename_map <- c(
    offensiveRebounds = "offensive_rebounds",
    defensiveRebounds = "defensive_rebounds"
  )
  for (old in names(rename_map)) {
    new <- rename_map[[old]]
    if (old %in% names(df) && !(new %in% names(df))) df[[new]] <- df[[old]]
  }

  schema_cols <- c(
    "game_id", "team_id", "season", "game_date",
    "player_id", "display_name", "jersey", "position",
    "starter", "did_not_play", "minutes_numeric", "points",
    "field_goals_made", "field_goals_attempted",
    "three_point_field_goals_made", "three_point_field_goals_attempted",
    "free_throws_made", "free_throws_attempted",
    "rebounds", "offensive_rebounds", "defensive_rebounds",
    "assists", "turnovers", "steals", "blocks", "fouls", "plus_minus",
    "fg_pct", "threep_pct", "ft_pct",
    "effective_field_goal_pct", "true_shooting_pct",
    "estimated_possessions", "points_per_estimated_possession",
    "points_per_minute"
  )
  df[, intersect(schema_cols, names(df)), drop = FALSE]
}

extract_betting_lines <- function(game_dir, meta) {
  df <- safe_read(file.path(game_dir, "pickcenter.rds"))
  if (is.null(df) || nrow(df) == 0L) return(NULL)

  df <- as_tibble(df)
  df$game_id <- meta$game_id

  # Pull home/away team ids from game header for convenience columns
  game_obj <- safe_read(file.path(game_dir, "game.rds"))
  home_id <- NA_character_
  away_id <- NA_character_
  if (!is.null(game_obj)) {
    comps <- game_obj$header$competitions
    if (!is.null(comps) && length(comps) > 0L) {
      for (cmp in comps[[1]]$competitors) {
        if (identical(cmp$homeAway, "home")) home_id <- as.character(cmp$team$id)
        if (identical(cmp$homeAway, "away")) away_id <- as.character(cmp$team$id)
      }
    }
  }
  df$home_team_id <- home_id
  df$away_team_id <- away_id

  num_cols <- c(
    "spread", "over_under", "over_odds", "under_odds",
    "home_spread_open", "home_spread_close",
    "away_spread_open", "away_spread_close",
    "home_spread_odds", "away_spread_odds",
    "home_moneyline", "away_moneyline",
    "home_moneyline_open", "home_moneyline_close",
    "away_moneyline_open", "away_moneyline_close",
    "total_over_open", "total_over_close",
    "total_under_open", "total_under_close"
  )
  for (nm in intersect(num_cols, names(df))) df[[nm]] <- as_num(df[[nm]])

  schema_cols <- c(
    "game_id", "provider_id", "provider_name", "details",
    "spread", "home_spread_open", "home_spread_close",
    "away_spread_open", "away_spread_close",
    "home_spread_odds", "away_spread_odds",
    "home_moneyline", "away_moneyline",
    "home_moneyline_open", "home_moneyline_close",
    "away_moneyline_open", "away_moneyline_close",
    "home_favorite", "away_favorite",
    "home_favorite_open", "away_favorite_open",
    "over_under", "over_odds", "under_odds",
    "total_over_open", "total_over_close",
    "total_under_open", "total_under_close",
    "home_team_id", "away_team_id"
  )
  df[, intersect(schema_cols, names(df)), drop = FALSE]
}

extract_win_probability <- function(game_dir, meta) {
  df <- safe_read(file.path(game_dir, "winprobability.rds"))
  if (is.null(df) || nrow(df) == 0L) return(NULL)

  df <- as_tibble(df)
  df$game_id <- meta$game_id
  for (nm in c("home_win_percentage", "tie_percentage")) {
    if (nm %in% names(df)) df[[nm]] <- as_num(df[[nm]])
  }

  schema_cols <- c(
    "game_id", "play_id",
    "home_team_id", "away_team_id",
    "home_win_percentage", "tie_percentage"
  )
  df[, intersect(schema_cols, names(df)), drop = FALSE]
}

extract_plays <- function(game_dir, meta) {
  df <- safe_read(file.path(game_dir, "plays.rds"))
  if (is.null(df) || nrow(df) == 0L) return(NULL)

  df <- as_tibble(df)
  df$game_id <- meta$game_id

  num_cols <- c(
    "period", "game_seconds", "away_score", "home_score",
    "score_value", "x_coordinate", "y_coordinate",
    "shot_distance", "points_attempted"
  )
  for (nm in intersect(num_cols, names(df))) df[[nm]] <- as_num(df[[nm]])

  schema_cols <- c(
    "game_id", "play_id", "sequence_number",
    "play_type_id", "play_type", "text", "short_description",
    "team_id", "period", "period_display", "clock", "game_seconds",
    "away_score", "home_score", "scoring_play", "score_value",
    "shooting_play", "x_coordinate", "y_coordinate",
    "shot_distance", "points_attempted", "wall_clock",
    "participant1_id", "participant2_id"
  )
  df[, intersect(schema_cols, names(df)), drop = FALSE]
}

extract_on_off <- function(game_dir, meta) {
  df <- safe_read(file.path(game_dir, "on_off.rds"))
  if (is.null(df) || nrow(df) == 0L) return(NULL)

  df <- as_tibble(df)
  df$game_id <- meta$game_id

  for (nm in c("start_time", "end_time", "minutes", "plus_minus")) {
    if (nm %in% names(df)) df[[nm]] <- as_num(df[[nm]])
  }

  schema_cols <- c(
    "game_id", "team_id", "player_id",
    "start_time", "end_time", "minutes", "plus_minus"
  )
  df[, intersect(schema_cols, names(df)), drop = FALSE]
}

extract_leaders <- function(game_dir, meta) {
  df <- safe_read(file.path(game_dir, "leaders.rds"))
  if (is.null(df) || nrow(df) == 0L) return(NULL)

  df <- as_tibble(df)
  df$game_id <- meta$game_id

  schema_cols <- c(
    "game_id", "team_id", "team_display_name",
    "category", "category_display",
    "player_id", "player_display_name",
    "display_value", "main_stat_value", "main_stat_label"
  )
  df[, intersect(schema_cols, names(df)), drop = FALSE]
}

# -----------------------------------------------------------------------------
# NULL coalesce operator (base R compatible)
# -----------------------------------------------------------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

# -----------------------------------------------------------------------------
# MAIN LOADER
# -----------------------------------------------------------------------------

cli_h1("ESPN DB → PostgreSQL Loader")
cli_alert_info("Connecting to {pg_config$dbname} at {pg_config$host}:{pg_config$port}")

con <- tryCatch(con_open(), error = function(e) {
  cli_abort("Failed to connect to PostgreSQL: {e$message}")
})
on.exit(dbDisconnect(con), add = TRUE)
cli_alert_success("Connected.")

for (season in SEASONS) {
  cli_h2("Season: {season}")

  game_dirs <- list_game_dirs(ESPN_DB_PATH, season)
  if (length(game_dirs) == 0L) {
    cli_alert_warning("No game dirs found for {season} in {ESPN_DB_PATH}")
    next
  }
  cli_alert_info("{length(game_dirs)} game directories found.")

  # Accumulators — collect across all games then batch-insert per table
  all_teams    <- tibble()
  all_games    <- tibble()
  all_team_box <- tibble()
  all_players  <- tibble()
  all_lines    <- tibble()
  all_wp       <- tibble()
  all_plays    <- tibble()
  all_on_off   <- tibble()
  all_leaders  <- tibble()

  pb <- cli_progress_bar("Processing games", total = length(game_dirs))

  for (gdir in game_dirs) {
    meta <- parse_game_path(gdir)

    all_teams    <- bind_rows(all_teams,    extract_teams(gdir))
    all_games    <- bind_rows(all_games,    extract_game_info(gdir, meta))
    all_team_box <- bind_rows(all_team_box, extract_team_box(gdir, meta))
    all_players  <- bind_rows(all_players,  extract_player_box(gdir, meta))
    all_lines    <- bind_rows(all_lines,    extract_betting_lines(gdir, meta))
    all_wp       <- bind_rows(all_wp,       extract_win_probability(gdir, meta))
    all_plays    <- bind_rows(all_plays,    extract_plays(gdir, meta))
    all_on_off   <- bind_rows(all_on_off,   extract_on_off(gdir, meta))
    all_leaders  <- bind_rows(all_leaders,  extract_leaders(gdir, meta))

    cli_progress_update()
  }
  cli_progress_done()

  # ---- TEAMS (deduplicate — many games share same teams) -------------------
  cli_alert_info("Inserting teams...")
  all_teams <- all_teams |>
    filter(!is.na(team_id), nzchar(team_id)) |>
    distinct(team_id, .keep_all = TRUE)

  n <- batch_insert(con, "teams", all_teams, "team_id")
  cli_alert_success("teams: {n} new rows inserted ({nrow(all_teams)} unique teams)")

  # ---- GAMES ---------------------------------------------------------------
  cli_alert_info("Inserting games...")
  all_games <- all_games |> filter(!is.na(game_id), nzchar(game_id))
  n <- batch_insert(con, "games", all_games, "game_id")
  cli_alert_success("games: {n} new rows inserted")

  # ---- TEAM BOX ------------------------------------------------------------
  cli_alert_info("Inserting team_box...")
  all_team_box <- all_team_box |> filter(!is.na(game_id), !is.na(team_id))
  n <- batch_insert(con, "team_box", all_team_box, c("game_id", "team_id"))
  cli_alert_success("team_box: {n} new rows inserted")

  # ---- PLAYER BOX ----------------------------------------------------------
  cli_alert_info("Inserting player_box...")
  all_players <- all_players |> filter(!is.na(game_id), !is.na(team_id), !is.na(player_id))
  n <- batch_insert(con, "player_box", all_players, c("game_id", "team_id", "player_id"))
  cli_alert_success("player_box: {n} new rows inserted")

  # ---- BETTING LINES -------------------------------------------------------
  cli_alert_info("Inserting betting_lines...")
  all_lines <- all_lines |> filter(!is.na(game_id), !is.na(provider_id))
  n <- batch_insert(con, "betting_lines", all_lines, c("game_id", "provider_id"))
  cli_alert_success("betting_lines: {n} new rows inserted")

  # ---- WIN PROBABILITY -----------------------------------------------------
  cli_alert_info("Inserting win_probability...")
  all_wp <- all_wp |> filter(!is.na(game_id))
  # No unique constraint on win_probability — use plain write to avoid dupes on re-run
  existing_games_wp <- dbGetQuery(
    con, "SELECT DISTINCT game_id FROM win_probability"
  )$game_id
  all_wp_new <- all_wp |> filter(!(game_id %in% existing_games_wp))
  if (nrow(all_wp_new) > 0L) {
    dbWriteTable(con, "win_probability", all_wp_new, append = TRUE)
  }
  cli_alert_success("win_probability: {nrow(all_wp_new)} rows inserted")

  # ---- PLAYS ---------------------------------------------------------------
  cli_alert_info("Inserting plays...")
  all_plays <- all_plays |> filter(!is.na(game_id))
  existing_games_plays <- dbGetQuery(
    con, "SELECT DISTINCT game_id FROM plays"
  )$game_id
  all_plays_new <- all_plays |> filter(!(game_id %in% existing_games_plays))
  if (nrow(all_plays_new) > 0L) {
    batches <- split(all_plays_new, ceiling(seq_len(nrow(all_plays_new)) / BATCH_SIZE))
    for (b in batches) dbWriteTable(con, "plays", b, append = TRUE)
  }
  cli_alert_success("plays: {nrow(all_plays_new)} rows inserted")

  # ---- ON/OFF --------------------------------------------------------------
  cli_alert_info("Inserting on_off...")
  all_on_off <- all_on_off |> filter(!is.na(game_id))
  existing_games_oo <- dbGetQuery(
    con, "SELECT DISTINCT game_id FROM on_off"
  )$game_id
  all_on_off_new <- all_on_off |> filter(!(game_id %in% existing_games_oo))
  if (nrow(all_on_off_new) > 0L) {
    dbWriteTable(con, "on_off", all_on_off_new, append = TRUE)
  }
  cli_alert_success("on_off: {nrow(all_on_off_new)} rows inserted")

  # ---- LEADERS -------------------------------------------------------------
  cli_alert_info("Inserting leaders...")
  all_leaders <- all_leaders |> filter(!is.na(game_id))
  existing_games_ldr <- dbGetQuery(
    con, "SELECT DISTINCT game_id FROM leaders"
  )$game_id
  all_leaders_new <- all_leaders |> filter(!(game_id %in% existing_games_ldr))
  if (nrow(all_leaders_new) > 0L) {
    dbWriteTable(con, "leaders", all_leaders_new, append = TRUE)
  }
  cli_alert_success("leaders: {nrow(all_leaders_new)} rows inserted")

  cli_alert_success("Season {season} complete.")
}

cli_h1("Load complete.")
