# =============================================================================
# ESPN utility functions: team directory and daily scoreboard
# =============================================================================


#' Get ESPN men's college basketball team directory
#'
#' Retrieves all D1 men's college basketball teams from ESPN's API, including
#' ESPN team IDs, display names, abbreviations, and conference membership.
#' ESPN team IDs are required for many other ESPN API calls and are not the
#' same as Torvik or Sports Reference team names.
#'
#' @param limit Integer. Maximum teams to fetch per API page. ESPN returns up
#'   to 1000 at once; default `1000` fetches everything in a single call.
#' @param timeout Numeric. Request timeout in seconds. Default `15`.
#'
#' @return A tibble with one row per team, containing:
#'   \describe{
#'     \item{team_id}{ESPN team ID (character). Use this in ESPN API calls and
#'       to join against `espn_game()` box score output.}
#'     \item{display_name}{Full team name (e.g. `"Duke Blue Devils"`).}
#'     \item{short_name}{Short name (e.g. `"Duke"`).}
#'     \item{abbreviation}{Abbreviation (e.g. `"DUKE"`).}
#'     \item{location}{School location (e.g. `"Duke"`).}
#'     \item{nickname}{Team nickname (e.g. `"Blue Devils"`).}
#'     \item{color}{Primary brand color as hex (e.g. `"003087"`).}
#'     \item{alternate_color}{Alternate brand color as hex.}
#'     \item{logo_url}{URL of the team logo PNG.}
#'     \item{conference_id}{ESPN conference ID.}
#'     \item{conference_name}{Conference display name.}
#'   }
#'
#' @details
#' ESPN team IDs are stable identifiers used throughout the ESPN ecosystem and
#' in `konenMCBB`'s ESPN pipeline. For example, Duke's ESPN team ID is `"150"`.
#' Use [espn_teams()] to look up the ID for any school before querying
#' game-level data.
#'
#' @examples
#' \dontrun{
#'   teams <- espn_teams()
#'
#'   # Find a specific team's ESPN ID
#'   teams |> dplyr::filter(grepl("Duke", display_name))
#'
#'   # All ACC teams
#'   teams |> dplyr::filter(conference_name == "ACC")
#'
#'   # Teams with their brand colors (useful for viz)
#'   teams |> dplyr::select(display_name, abbreviation, color, logo_url)
#' }
#'
#' @seealso [espn_scoreboard()] for daily game listings, [espn_game()] to
#'   fetch game data using ESPN game IDs.
#'
#' @export
espn_teams <- function(limit = 1000L, timeout = 15) {

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/basketball/",
    "mens-college-basketball/teams?limit=", as.integer(limit)
  )

  req <- httr2::request(url) |>
    httr2::req_headers(
      Accept       = "application/json",
      `User-Agent` = "konenMCBB R package (github.com/charleskonen1/konenMCBB)"
    ) |>
    httr2::req_timeout(timeout)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) stop("Failed to reach ESPN API: ", conditionMessage(e))
  )
  httr2::resp_check_status(resp)

  raw <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)

  sports    <- raw$sports
  leagues   <- if (!is.null(sports) && length(sports) > 0) sports[[1]]$leagues else NULL
  teams_raw <- if (!is.null(leagues) && length(leagues) > 0) leagues[[1]]$teams else NULL

  if (is.null(teams_raw) || length(teams_raw) == 0) {
    stop("No team data found in ESPN API response.")
  }

  out <- purrr::map_dfr(teams_raw, function(entry) {
    t    <- entry$team
    conf <- t$groups

    tibble::tibble(
      team_id          = as.character(.null_or(t$id,                  NA_character_)),
      display_name     = as.character(.null_or(t$displayName,         NA_character_)),
      short_name       = as.character(.null_or(t$shortDisplayName,    NA_character_)),
      abbreviation     = as.character(.null_or(t$abbreviation,        NA_character_)),
      location         = as.character(.null_or(t$location,            NA_character_)),
      nickname         = as.character(.null_or(t$nickname,            NA_character_)),
      color            = as.character(.null_or(t$color,               NA_character_)),
      alternate_color  = as.character(.null_or(t$alternateColor,      NA_character_)),
      logo_url         = {
        logos <- t$logos
        if (!is.null(logos) && length(logos) > 0) as.character(.null_or(logos[[1]]$href, NA_character_)) else NA_character_
      },
      conference_id    = as.character(.null_or(.get_in(conf, "id"),          NA_character_)),
      conference_name  = as.character(.null_or(.get_in(conf, "shortName"),   NA_character_))
    )
  })

  dplyr::filter(out, !is.na(.data$team_id), nzchar(.data$team_id))
}


#' Get ESPN daily college basketball scoreboard
#'
#' Returns all men's college basketball games on a given date from ESPN's
#' scoreboard API — with scores, game status, team IDs, and ESPN game IDs.
#' Works for past, present, and future dates.
#'
#' @param date Date or character. The date to fetch (e.g. `Sys.Date()`,
#'   `"2024-11-11"`, or `as.Date("2025-03-15")`). Defaults to today.
#' @param groups Character. ESPN group filter. `"50"` (default) returns D1
#'   games only. Use `"100"` for all games including lower divisions.
#' @param timeout Numeric. Request timeout in seconds. Default `15`.
#'
#' @return A tibble with one row per game, containing:
#'   \describe{
#'     \item{game_id}{ESPN event ID (use with [espn_game()] to pull full data).}
#'     \item{date}{Game date as Date.}
#'     \item{game_datetime}{Tip-off time as UTC character string.}
#'     \item{status}{Game status: `"scheduled"`, `"in_progress"`, or `"final"`.}
#'     \item{period}{Current period (useful for in-progress games).}
#'     \item{clock}{Game clock string for in-progress games.}
#'     \item{home_team_id, away_team_id}{ESPN team IDs (join with [espn_teams()]).}
#'     \item{home_team_name, away_team_name}{Team display names.}
#'     \item{home_score, away_score}{Current or final scores.}
#'     \item{home_winner, away_winner}{Logical; `TRUE` if that team won.}
#'     \item{neutral_site}{Logical; `TRUE` for neutral-site games.}
#'     \item{conference_game}{Logical; `TRUE` for conference matchups.}
#'     \item{venue_name}{Arena name.}
#'     \item{broadcast}{TV/streaming broadcast label (e.g. `"ESPN"`, `"ESPN2"`).}
#'   }
#'
#' @examples
#' \dontrun{
#'   # Today's games
#'   scoreboard <- espn_scoreboard()
#'
#'   # A specific date
#'   espn_scoreboard("2024-03-15")
#'
#'   # Games still in progress right now
#'   espn_scoreboard() |> dplyr::filter(status == "in_progress")
#'
#'   # Get game IDs to pass to espn_game()
#'   ids <- espn_scoreboard("2024-11-11")$game_id
#'   game <- espn_game(ids[1])
#'
#'   # Loop and fetch all games on a date
#'   board <- espn_scoreboard("2024-11-11")
#'   games <- lapply(board$game_id, espn_game)
#' }
#'
#' @seealso [espn_game()] to fetch full game data, [espn_teams()] for team
#'   IDs, [espn_process_day()] to save all games for a date to local DB.
#'
#' @export
espn_scoreboard <- function(
    date    = Sys.Date(),
    groups  = "50",
    timeout = 15
) {

  date_obj  <- as.Date(date)
  date_str  <- format(date_obj, "%Y%m%d")

  url <- paste0(
    "https://site.api.espn.com/apis/site/v2/sports/basketball/",
    "mens-college-basketball/scoreboard",
    "?dates=", date_str,
    "&groups=", as.character(groups),
    "&limit=500"
  )

  req <- httr2::request(url) |>
    httr2::req_headers(
      Accept       = "application/json",
      `User-Agent` = "konenMCBB R package (github.com/charleskonen1/konenMCBB)"
    ) |>
    httr2::req_timeout(timeout)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) stop("Failed to reach ESPN scoreboard API: ", conditionMessage(e))
  )
  httr2::resp_check_status(resp)

  raw    <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = FALSE)
  events <- raw$events

  if (is.null(events) || length(events) == 0) {
    message("No games found for ", format(date_obj), ".")
    return(tibble::tibble())
  }

  out <- purrr::map_dfr(events, function(ev) {

    comp  <- if (!is.null(ev$competitions) && length(ev$competitions) > 0) ev$competitions[[1]] else list()
    comps <- if (!is.null(comp$competitors)) comp$competitors else list()

    home_team_id   <- NA_character_
    away_team_id   <- NA_character_
    home_team_name <- NA_character_
    away_team_name <- NA_character_
    home_score     <- NA_integer_
    away_score     <- NA_integer_
    home_winner    <- NA
    away_winner    <- NA

    for (cmp in comps) {
      ha  <- .null_or(cmp$homeAway, NA_character_)
      tid <- as.character(.null_or(.get_in(cmp, "team", "id"), NA_character_))
      tnm <- as.character(.null_or(.get_in(cmp, "team", "displayName"), NA_character_))
      sc  <- suppressWarnings(as.integer(.null_or(cmp$score, NA_integer_)))
      win <- isTRUE(as.logical(.null_or(cmp$winner, NA)))

      if (identical(ha, "home")) {
        home_team_id   <- tid
        home_team_name <- tnm
        home_score     <- sc
        home_winner    <- win
      }
      if (identical(ha, "away")) {
        away_team_id   <- tid
        away_team_name <- tnm
        away_score     <- sc
        away_winner    <- win
      }
    }

    # Status
    status_type <- .null_or(.get_in(comp, "status", "type", "name"), NA_character_)
    status <- dplyr::case_when(
      grepl("Final|Forfeit", status_type, ignore.case = TRUE) ~ "final",
      grepl("InProgress|Half", status_type, ignore.case = TRUE) ~ "in_progress",
      TRUE ~ "scheduled"
    )
    period <- suppressWarnings(as.integer(.null_or(.get_in(comp, "status", "period"), NA_integer_)))
    clock  <- as.character(.null_or(.get_in(comp, "status", "displayClock"), NA_character_))

    # Venue
    venue_name <- as.character(.null_or(.get_in(comp, "venue", "fullName"), NA_character_))

    # Broadcast
    broadcasts <- comp$broadcasts
    broadcast  <- if (!is.null(broadcasts) && length(broadcasts) > 0) {
      mkt <- broadcasts[[1]]$market
      nms <- broadcasts[[1]]$names
      if (!is.null(nms) && length(nms) > 0) paste(unlist(nms), collapse = "/") else NA_character_
    } else NA_character_

    tibble::tibble(
      game_id          = as.character(.null_or(ev$id,  NA_character_)),
      date             = date_obj,
      game_datetime    = as.character(.null_or(ev$date, NA_character_)),
      status           = status,
      period           = period,
      clock            = clock,
      home_team_id     = home_team_id,
      away_team_id     = away_team_id,
      home_team_name   = home_team_name,
      away_team_name   = away_team_name,
      home_score       = home_score,
      away_score       = away_score,
      home_winner      = home_winner,
      away_winner      = away_winner,
      neutral_site     = isTRUE(as.logical(.null_or(comp$neutralSite, FALSE))),
      conference_game  = isTRUE(as.logical(.null_or(comp$conferenceCompetition, NA))),
      venue_name       = venue_name,
      broadcast        = broadcast
    )
  })

  out
}
