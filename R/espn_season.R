# =============================================================================
# ESPN season-level aggregation helpers
#
# These functions read from the local ESPN database and combine all game data
# for a season into single flat tibbles — useful for offline analysis without
# needing Postgres.
# =============================================================================

# Coerce box-score stat columns that ESPN sometimes returns as character so
# that bind_rows across many games doesn't fail on type conflicts (e.g. one
# game has `points` <chr> and another <dbl>).
.espn_coerce_box_numeric <- function(df) {
  num_cols <- intersect(
    c(
      "points",
      "field_goals_made", "field_goals_attempted",
      "three_point_field_goals_made", "three_point_field_goals_attempted",
      "free_throws_made", "free_throws_attempted",
      "rebounds", "totalRebounds",
      "offensiveRebounds", "offensive_rebounds",
      "defensiveRebounds", "defensive_rebounds",
      "assists", "steals", "blocks", "turnovers",
      "teamTurnovers", "totalTurnovers",
      "fouls", "technicalFouls", "flagrantFouls",
      "fastBreakPoints", "pointsInPaint", "turnoverPoints", "largestLead",
      "minutes_numeric", "estimated_possessions",
      "points_per_estimated_possession", "plus_minus"
    ),
    names(df)
  )
  for (nm in num_cols) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  df
}


#' Load all team box scores for a season from the local ESPN database
#'
#' Reads every saved `box_team_stats.rds` file for a season, attaches
#' game metadata (game ID, date, season), and returns one combined tibble.
#' This gives you a full season of team-level box score data in one call —
#' ready for filtering, aggregation, and analysis.
#'
#' @param season Character. Season label (e.g. `"2024-25"`).
#' @param base_path Character. ESPN database root path. Defaults to
#'   `getOption("konenMCBB.espn_db_path")`. Set with [espn_set_db_path()].
#' @param dates Optional Date vector or character vector of `"YYYY-MM-DD"`
#'   dates to include. If `NULL` (default), all available dates are loaded.
#'
#' @return A tibble with one row per team per game, combining:
#'   \describe{
#'     \item{season}{Season label (e.g. `"2024-25"`).}
#'     \item{game_date}{Date of the game.}
#'     \item{game_id}{ESPN event ID.}
#'     \item{team_id}{ESPN team ID.}
#'     \item{home_away}{`"home"` or `"away"`.}
#'     \item{points, field_goals_made, field_goals_attempted, ...}{Standard
#'       box score counting stats.}
#'     \item{effective_field_goal_pct, true_shooting_pct, eff, pace, ...}{
#'       Advanced metrics computed by the ESPN pipeline.}
#'   }
#'   Returns an empty tibble if no data is found for the season.
#'
#' @examples
#' \dontrun{
#'   espn_set_db_path("~/data/espn_db")
#'
#'   # Full season box scores
#'   box <- espn_season_box("2024-25")
#'
#'   # Average offensive efficiency by team
#'   box |>
#'     dplyr::group_by(team_id) |>
#'     dplyr::summarise(
#'       games    = dplyr::n(),
#'       avg_eff  = mean(eff, na.rm = TRUE),
#'       avg_efg  = mean(effective_field_goal_pct, na.rm = TRUE),
#'       avg_pace = mean(pace, na.rm = TRUE)
#'     ) |>
#'     dplyr::arrange(dplyr::desc(avg_eff))
#'
#'   # Load only games from a specific date range
#'   dates <- seq(as.Date("2025-01-01"), as.Date("2025-03-01"), by = "day")
#'   box_conf <- espn_season_box("2024-25", dates = dates)
#' }
#'
#' @seealso [espn_season_players()] for player-level data,
#'   [espn_process_day()] to collect data, [espn_load_game()] for a single game.
#'
#' @export
espn_season_box <- function(
    season,
    base_path = .espn_db_base(),
    dates     = NULL
) {

  if (missing(season)) stop("`season` is required (e.g. '2024-25').")
  season    <- as.character(season)
  season_dir <- file.path(base_path, season)

  if (!dir.exists(season_dir)) {
    message("Season directory not found: ", season_dir)
    return(tibble::tibble())
  }

  day_dirs  <- list.dirs(season_dir, full.names = TRUE, recursive = FALSE)
  day_names <- basename(day_dirs)
  day_dates <- suppressWarnings(as.Date(day_names))
  valid     <- !is.na(day_dates)
  day_dirs  <- day_dirs[valid]
  day_dates <- day_dates[valid]

  if (!is.null(dates)) {
    dates_vec <- as.Date(dates)
    keep      <- day_dates %in% dates_vec
    day_dirs  <- day_dirs[keep]
    day_dates <- day_dates[keep]
  }

  if (length(day_dirs) == 0) {
    message("No game data found for season '", season, "'.")
    return(tibble::tibble())
  }

  out <- purrr::map2_dfr(day_dirs, day_dates, function(day_dir, day_date) {
    game_dirs <- list.dirs(day_dir, full.names = TRUE, recursive = FALSE)
    purrr::map_dfr(game_dirs, function(gd) {
      path <- file.path(gd, "box_team_stats.rds")
      if (!file.exists(path)) return(tibble::tibble())
      df <- tryCatch(readRDS(path), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
      df <- .espn_coerce_box_numeric(df)
      df$season    <- season
      df$game_date <- day_date
      df$game_id   <- basename(gd)
      tibble::as_tibble(df)
    })
  })

  # Reorder key columns to front
  key_cols <- c("season", "game_date", "game_id", "team_id", "home_away")
  other    <- setdiff(names(out), key_cols)
  out      <- dplyr::select(out, dplyr::any_of(key_cols), dplyr::any_of(other))

  out
}


#' Load all player box scores for a season from the local ESPN database
#'
#' Reads every saved `box_players.rds` file for a season and returns one
#' combined tibble with one row per player per game. Includes raw box score
#' stats, advanced metrics, and plus/minus.
#'
#' @param season Character. Season label (e.g. `"2024-25"`).
#' @param base_path Character. ESPN database root path. Defaults to
#'   `getOption("konenMCBB.espn_db_path")`. Set with [espn_set_db_path()].
#' @param dates Optional Date vector or character vector of `"YYYY-MM-DD"`
#'   dates to include. If `NULL` (default), all available dates are loaded.
#' @param active_only Logical. If `TRUE` (default), excludes rows where
#'   `did_not_play == TRUE`.
#'
#' @return A tibble with one row per player per game, containing:
#'   \describe{
#'     \item{season}{Season label.}
#'     \item{game_date}{Date of the game.}
#'     \item{game_id}{ESPN event ID.}
#'     \item{team_id}{ESPN team ID.}
#'     \item{player_id}{ESPN player ID.}
#'     \item{display_name}{Player name.}
#'     \item{position}{Position abbreviation.}
#'     \item{starter}{Logical; `TRUE` if the player started.}
#'     \item{did_not_play}{Logical; `TRUE` if the player did not play.}
#'     \item{minutes_numeric}{Minutes played as a decimal.}
#'     \item{points, field_goals_made, ...}{Counting stats.}
#'     \item{effective_field_goal_pct, true_shooting_pct, ...}{Advanced metrics.}
#'     \item{plus_minus}{Plus/minus for the game.}
#'   }
#'
#' @examples
#' \dontrun{
#'   espn_set_db_path("~/data/espn_db")
#'
#'   players <- espn_season_players("2024-25")
#'
#'   # Season averages for all players (min 10 games)
#'   players |>
#'     dplyr::group_by(player_id, display_name, team_id) |>
#'     dplyr::summarise(
#'       games         = dplyr::n(),
#'       avg_pts       = mean(points, na.rm = TRUE),
#'       avg_ts        = mean(true_shooting_pct, na.rm = TRUE),
#'       avg_plus_minus = mean(plus_minus, na.rm = TRUE),
#'       .groups = "drop"
#'     ) |>
#'     dplyr::filter(games >= 10) |>
#'     dplyr::arrange(dplyr::desc(avg_plus_minus))
#'
#'   # Starters only
#'   starters <- espn_season_players("2024-25") |>
#'     dplyr::filter(starter == TRUE)
#' }
#'
#' @seealso [espn_season_box()] for team-level data,
#'   [espn_process_day()] to collect game data.
#'
#' @export
espn_season_players <- function(
    season,
    base_path   = .espn_db_base(),
    dates       = NULL,
    active_only = TRUE
) {

  if (missing(season)) stop("`season` is required (e.g. '2024-25').")
  season     <- as.character(season)
  season_dir <- file.path(base_path, season)

  if (!dir.exists(season_dir)) {
    message("Season directory not found: ", season_dir)
    return(tibble::tibble())
  }

  day_dirs  <- list.dirs(season_dir, full.names = TRUE, recursive = FALSE)
  day_names <- basename(day_dirs)
  day_dates <- suppressWarnings(as.Date(day_names))
  valid     <- !is.na(day_dates)
  day_dirs  <- day_dirs[valid]
  day_dates <- day_dates[valid]

  if (!is.null(dates)) {
    dates_vec <- as.Date(dates)
    keep      <- day_dates %in% dates_vec
    day_dirs  <- day_dirs[keep]
    day_dates <- day_dates[keep]
  }

  if (length(day_dirs) == 0) {
    message("No game data found for season '", season, "'.")
    return(tibble::tibble())
  }

  out <- purrr::map2_dfr(day_dirs, day_dates, function(day_dir, day_date) {
    game_dirs <- list.dirs(day_dir, full.names = TRUE, recursive = FALSE)
    purrr::map_dfr(game_dirs, function(gd) {
      path <- file.path(gd, "box_players.rds")
      if (!file.exists(path)) return(tibble::tibble())
      df <- tryCatch(readRDS(path), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
      df <- .espn_coerce_box_numeric(df)
      df$season    <- season
      df$game_date <- day_date
      df$game_id   <- basename(gd)
      tibble::as_tibble(df)
    })
  })

  if (nrow(out) == 0) return(out)

  if (isTRUE(active_only) && "did_not_play" %in% names(out)) {
    out <- dplyr::filter(out, !isTRUE(.data$did_not_play))
  }

  key_cols <- c("season", "game_date", "game_id", "team_id",
                "player_id", "display_name", "position", "starter",
                "did_not_play", "minutes_numeric")
  other    <- setdiff(names(out), key_cols)
  out      <- dplyr::select(out, dplyr::any_of(key_cols), dplyr::any_of(other))

  out
}


#' Summarise team season stats from the local ESPN database
#'
#' Aggregates all game-level team box scores for a season into per-team
#' season totals and averages. A quick way to rank teams by any box score
#' or efficiency metric without Postgres.
#'
#' @param season Character. Season label (e.g. `"2024-25"`).
#' @param base_path Character. ESPN database root path.
#' @param min_games Integer. Minimum games played to include a team. Default `5`.
#'
#' @return A tibble with one row per team, containing season totals and
#'   per-game averages for points, rebounds, assists, turnovers, efficiency,
#'   eFG%, TS%, pace, and more.
#'
#' @examples
#' \dontrun{
#'   espn_set_db_path("~/data/espn_db")
#'
#'   summary <- espn_team_season_summary("2024-25")
#'   summary |> dplyr::arrange(dplyr::desc(avg_eff))
#' }
#'
#' @seealso [espn_season_box()] for game-level data.
#'
#' @export
espn_team_season_summary <- function(
    season,
    base_path = .espn_db_base(),
    min_games = 5L
) {

  # Use espn_team_games(): it adds points_allowed, margin, and won (which a
  # plain box-score load does not) and coerces stat columns to numeric.
  box <- espn_team_games(season, base_path = base_path)
  if (nrow(box) == 0) return(tibble::tibble())

  safe_num <- function(x) suppressWarnings(as.numeric(x))

  box <- dplyr::mutate(box,
    dplyr::across(
      dplyr::where(is.character) & !dplyr::any_of(c("team_id", "home_away", "game_id", "season")),
      safe_num
    )
  )

  # espn_team_games() returns ESPN's camelCase rebound names; normalise to the
  # snake_case names this summary expects.
  rename_map <- c(
    rebounds          = "totalRebounds",
    offensive_rebounds = "offensiveRebounds",
    defensive_rebounds = "defensiveRebounds"
  )
  for (new_nm in names(rename_map)) {
    old_nm <- rename_map[[new_nm]]
    if (!new_nm %in% names(box) && old_nm %in% names(box)) {
      box[[new_nm]] <- box[[old_nm]]
    }
  }

  summary_tbl <- box |>
    dplyr::group_by(.data$team_id) |>
    dplyr::summarise(
      games              = dplyr::n(),
      wins               = sum(.data$points > .data$points_allowed, na.rm = TRUE),
      losses             = sum(.data$points < .data$points_allowed, na.rm = TRUE),
      # Per-game averages
      avg_pts            = mean(.data$points,               na.rm = TRUE),
      avg_pts_allowed    = mean(.data$points_allowed,       na.rm = TRUE),
      avg_margin         = mean(.data$margin,               na.rm = TRUE),
      avg_fgm            = mean(.data$field_goals_made,     na.rm = TRUE),
      avg_fga            = mean(.data$field_goals_attempted, na.rm = TRUE),
      avg_3pm            = mean(.data$three_point_field_goals_made,     na.rm = TRUE),
      avg_3pa            = mean(.data$three_point_field_goals_attempted, na.rm = TRUE),
      avg_ftm            = mean(.data$free_throws_made,     na.rm = TRUE),
      avg_fta            = mean(.data$free_throws_attempted, na.rm = TRUE),
      avg_reb            = mean(.data$rebounds,             na.rm = TRUE),
      avg_oreb           = mean(.data$offensive_rebounds,   na.rm = TRUE),
      avg_dreb           = mean(.data$defensive_rebounds,   na.rm = TRUE),
      avg_ast            = mean(.data$assists,              na.rm = TRUE),
      avg_tov            = mean(.data$turnovers,            na.rm = TRUE),
      avg_stl            = mean(.data$steals,               na.rm = TRUE),
      avg_blk            = mean(.data$blocks,               na.rm = TRUE),
      avg_poss           = mean(.data$estimated_possessions, na.rm = TRUE),
      # Efficiency metrics
      avg_eff            = mean(.data$eff,                       na.rm = TRUE),
      avg_efg            = mean(.data$effective_field_goal_pct,  na.rm = TRUE),
      avg_ts             = mean(.data$true_shooting_pct,         na.rm = TRUE),
      avg_pace           = mean(.data$pace,                      na.rm = TRUE),
      avg_ftar           = mean(.data$ftar,                      na.rm = TRUE),
      avg_threepar       = mean(.data$threepar,                  na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$games >= min_games) |>
    dplyr::mutate(
      win_pct = round(.data$wins / .data$games, 3),
      dplyr::across(dplyr::where(is.numeric), ~ round(.x, 2))
    ) |>
    dplyr::arrange(dplyr::desc(.data$avg_eff))

  summary_tbl
}
