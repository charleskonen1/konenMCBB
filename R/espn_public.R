# =============================================================================
# Public-facing ESPN functions
#
# These wrappers expose the internal ESPN data pipeline to end users.
# The underlying fetch/parse logic lives in espn_fetch.R, espn_parse_game.R,
# espn_process_day.R, and espn_game_object.R.
# =============================================================================


#' Set the ESPN local database path
#'
#' Configures the root directory where `konenMCBB` stores and reads ESPN game
#' data. Set this once per session (or in your `.Rprofile`) before calling any
#' ESPN data functions.
#'
#' @param path Character. Full path to the ESPN database root. The package
#'   expects the structure `<path>/<season>/<YYYY-MM-DD>/<game_id>/`.
#'
#' @return The path, invisibly.
#'
#' @examples
#' \dontrun{
#'   # Point to a folder on your machine
#'   espn_set_db_path("~/data/espn_db")
#'
#'   # Or within a project
#'   espn_set_db_path(here::here("data/espn_db"))
#' }
#'
#' @export
espn_set_db_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("`path` must be a non-empty single character string.")
  }
  options(konenMCBB.espn_db_path = path)
  invisible(path)
}


#' Fetch and parse a single ESPN game
#'
#' Retrieves a game from ESPN's public API and returns all parsed data frames
#' in a named list. Optionally saves to the local ESPN database for reuse.
#'
#' @param game_id Character or numeric. ESPN event (game) ID. These can be
#'   found in ESPN game URLs: `https://www.espn.com/mens-college-basketball/game/_/gameId/401700444`.
#' @param save Logical. If `TRUE`, save the raw JSON and parsed data frames
#'   to the local ESPN database. Requires `season` and optionally `date`.
#'   Default `FALSE`.
#' @param season Character. Season label (e.g. `"2024-25"`). Required if
#'   `save = TRUE`.
#' @param date Date or character `"YYYY-MM-DD"`. Game date. Required if
#'   `save = TRUE`.
#' @param base_path Character. ESPN database root path. Defaults to
#'   `getOption("konenMCBB.espn_db_path")`. Set with [espn_set_db_path()].
#'
#' @return A named list with the following tibble elements:
#'   \describe{
#'     \item{`box_team_stats`}{Team-level box score with advanced metrics (2 rows).}
#'     \item{`box_players`}{Player-level box score with advanced metrics.}
#'     \item{`game_info`}{Venue, attendance, and officials.}
#'     \item{`leaders`}{Stat leaders per category per team.}
#'     \item{`winprobability`}{Win probability by play.}
#'     \item{`pickcenter`}{Betting lines and spreads per provider.}
#'     \item{`plays`}{Full play-by-play with shot coordinates.}
#'     \item{`on_off`}{Player rotation stints with plus/minus.}
#'   }
#'
#' @examples
#' \dontrun{
#'   game <- espn_game("401700444")
#'
#'   # Team box score
#'   game$box_team_stats
#'
#'   # Player box score
#'   game$box_players
#'
#'   # Win probability chart data
#'   game$winprobability
#'
#'   # Save to local DB for later use
#'   espn_set_db_path("~/data/espn_db")
#'   game <- espn_game("401700444", save = TRUE, season = "2024-25", date = "2024-11-11")
#' }
#'
#' @seealso [espn_process_day()] to fetch and save all games for a date,
#'   [espn_rankings_summary()] to build team rankings from the local DB.
#'
#' @export
espn_game <- function(
    game_id,
    save      = FALSE,
    season    = NULL,
    date      = NULL,
    base_path = .espn_db_base()
) {
  if (missing(game_id) || length(game_id) != 1L) {
    stop("`game_id` must be a single value.")
  }
  game_id <- as.character(game_id)

  if (isTRUE(save)) {
    if (is.null(season) || !nzchar(as.character(season))) {
      stop("`season` must be provided when `save = TRUE` (e.g. '2024-25').")
    }
    if (is.null(date)) {
      stop("`date` must be provided when `save = TRUE` (e.g. '2024-11-11').")
    }
    date     <- as.Date(date)
    game_dir <- .espn_db_game_dir(base_path, as.character(season), date, game_id)
    .espn_process_one_game(game_id, game_dir, sleep_sec = 0)
    game_obj <- .espn_load_game_object(game_dir)
  } else {
    game_obj <- .espn_fetch_summary(game_id)
  }

  # Parse all data frames
  box_players_df <- .espn_parse_box_players(game_obj)
  on_off_df      <- .espn_parse_on_off(game_obj, players_df = box_players_df)

  list(
    box_team_stats  = .espn_parse_box_team_stats(game_obj),
    box_players     = box_players_df,
    game_info       = .espn_parse_game_info(game_obj),
    leaders         = .espn_parse_leaders(game_obj),
    winprobability  = .espn_parse_winprobability(game_obj),
    pickcenter      = .espn_parse_pickcenter(game_obj),
    plays           = .espn_parse_plays(game_obj),
    on_off          = on_off_df
  )
}


#' Fetch and save all ESPN games for a given date
#'
#' Scrapes ESPN for all D1 men's college basketball game IDs on a given date,
#' then fetches, parses, and saves each game to the local ESPN database. Safe
#' to re-run — already-fetched games are skipped (raw JSON is reused).
#'
#' @param date Date or character `"YYYY-MM-DD"`. The game date to process.
#' @param season Character. Season label, e.g. `"2024-25"`.
#' @param game_ids Optional character vector of ESPN event IDs. If `NULL`
#'   (default), game IDs are scraped from the ESPN scoreboard for that date.
#' @param base_path Character. ESPN database root. Defaults to
#'   `getOption("konenMCBB.espn_db_path")`. Set with [espn_set_db_path()].
#' @param sleep_sec Numeric. Base sleep between requests (randomised by +0–0.5s)
#'   to avoid rate limiting. Default `0.5`.
#' @param espn_group Character. ESPN scoreboard group ID. `"50"` (default) is
#'   all D1; `"2"` is all divisions.
#'
#' @return Invisible character vector of processed game IDs.
#'
#' @examples
#' \dontrun{
#'   espn_set_db_path("~/data/espn_db")
#'
#'   # Fetch all D1 games on a specific date
#'   espn_process_day("2024-11-11", season = "2024-25")
#'
#'   # Process a specific set of game IDs
#'   espn_process_day(
#'     date     = "2024-11-11",
#'     season   = "2024-25",
#'     game_ids = c("401700444", "401700453")
#'   )
#'
#'   # Loop over a date range
#'   dates <- seq(as.Date("2024-11-11"), as.Date("2024-11-15"), by = "day")
#'   for (d in as.character(dates)) {
#'     espn_process_day(d, season = "2024-25")
#'   }
#' }
#'
#' @seealso [espn_game()] for a single game, [espn_rankings_summary()] to build
#'   team rankings after populating the local database.
#'
#' @export
espn_process_day <- function(
    date,
    season,
    game_ids   = NULL,
    base_path  = .espn_db_base(),
    sleep_sec  = 0.5,
    espn_group = "50"
) {
  if (missing(date) || missing(season)) {
    stop("Both `date` and `season` are required.")
  }
  .espn_process_day(
    date       = date,
    season     = season,
    game_ids   = game_ids,
    base_path  = base_path,
    sleep_sec  = sleep_sec,
    espn_group = espn_group
  )
}


#' Load a parsed ESPN game from the local database
#'
#' Reads previously saved game data from the local ESPN database without making
#' any network requests. Useful for fast offline analysis after data has been
#' collected with [espn_process_day()].
#'
#' @param game_id Character or numeric. ESPN event (game) ID.
#' @param season Character. Season label (e.g. `"2024-25"`).
#' @param date Date or character `"YYYY-MM-DD"`. Game date.
#' @param base_path Character. ESPN database root path.
#'
#' @return A named list identical in structure to the return value of
#'   [espn_game()].
#'
#' @examples
#' \dontrun{
#'   espn_set_db_path("~/data/espn_db")
#'   game <- espn_load_game("401700444", season = "2024-25", date = "2024-11-11")
#'   game$box_team_stats
#' }
#'
#' @export
espn_load_game <- function(
    game_id,
    season,
    date,
    base_path = .espn_db_base()
) {
  if (missing(game_id) || length(game_id) != 1L) stop("`game_id` must be a single value.")
  if (missing(season))  stop("`season` is required (e.g. '2024-25').")
  if (missing(date))    stop("`date` is required (e.g. '2024-11-11').")

  game_id  <- as.character(game_id)
  date     <- as.Date(date)
  game_dir <- .espn_db_game_dir(base_path, as.character(season), date, game_id)

  if (!dir.exists(game_dir)) {
    stop(
      "Game directory not found: ", game_dir, "\n",
      "Have you run `espn_process_day()` for this date and season?"
    )
  }

  safe_rds <- function(nm) {
    path <- file.path(game_dir, paste0(nm, ".rds"))
    if (file.exists(path)) readRDS(path) else tibble::tibble()
  }

  list(
    box_team_stats = safe_rds("box_team_stats"),
    box_players    = safe_rds("box_players"),
    game_info      = safe_rds("game_info"),
    leaders        = safe_rds("leaders"),
    winprobability = safe_rds("winprobability"),
    pickcenter     = safe_rds("pickcenter"),
    plays          = safe_rds("plays"),
    on_off         = safe_rds("on_off")
  )
}
