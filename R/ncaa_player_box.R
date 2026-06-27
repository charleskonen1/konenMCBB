#' Get NCAA player box score
#'
#' Retrieves player-level box score statistics for a given NCAA contest ID from
#' the NCAA's GraphQL API. Returns one row per player per team.
#'
#' @param game_id NCAA contest ID. The numeric ID found in NCAA.com game URLs
#'   (e.g. for `https://www.ncaa.com/game/6262407/boxscore`, use `"6262407"`).
#'   Can be character or numeric.
#'
#' @return A tibble with one row per player, containing:
#'   \describe{
#'     \item{team_id}{NCAA team identifier.}
#'     \item{number}{Jersey number.}
#'     \item{firstName, lastName}{Player name components.}
#'     \item{position}{Position abbreviation.}
#'     \item{minutesPlayed}{Minutes played as a string (e.g. `"32:14"`).}
#'     \item{year}{Academic year (e.g. `"So"`).}
#'     \item{elig}{Eligibility label.}
#'     \item{starter}{Logical; `TRUE` if the player started.}
#'     \item{fieldGoalsMade, fieldGoalsAttempted}{FG totals.}
#'     \item{freeThrowsMade, freeThrowsAttempted}{FT totals.}
#'     \item{threePointsMade, threePointsAttempted}{3-point totals.}
#'     \item{offensiveRebounds, totalRebounds}{Rebound totals.}
#'     \item{assists, turnovers, personalFouls, steals, blockedShots}{Other counting stats.}
#'     \item{points}{Points scored.}
#'   }
#'
#' @examples
#' \dontrun{
#'   box <- ncaa_player_box("6262407")
#'   head(box)
#' }
#'
#' @export
ncaa_player_box <- function(game_id) {

  if (missing(game_id) || length(game_id) != 1) {
    stop("`game_id` must be a single value.")
  }

  game_id <- as.character(game_id)

  query_url <- paste0(
    "https://sdataprod.ncaa.com/",
    "?meta=NCAA_GetGamecenterBoxscoreBasketballById_web",
    "&extensions=%7B%22persistedQuery%22%3A%7B%22version%22%3A1%2C",
    "%22sha256Hash%22%3A%224a7fa26398db33de3ff51402a90eb5f25acef001cca28d239fe5361315d1419a%22%7D%7D",
    "&variables=%7B%22contestId%22%3A%22", game_id,
    "%22%2C%22staticTestEnv%22%3Anull%7D"
  )

  req <- httr2::request(query_url) |>
    httr2::req_headers(
      Accept       = "application/json",
      `User-Agent` = "konenMCBB R package (github.com/charleskonen1/konenMCBB)",
      Referer      = paste0("https://www.ncaa.com/game/", game_id, "/boxscore")
    ) |>
    httr2::req_timeout(20)

  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)

  txt <- httr2::resp_body_string(resp)
  j   <- jsonlite::fromJSON(txt, flatten = TRUE)

  team_box <- j$data$boxscore$teamBoxscore

  if (is.null(team_box)) {
    stop("No box score data found for game_id: ", game_id)
  }

  team_df <- tibble::as_tibble(team_box)
  box_df  <- dplyr::select(team_df, teamId, playerStats)
  box_df  <- tidyr::unnest(box_df, cols = playerStats)

  box_df <- dplyr::select(
    box_df,
    team_id            = teamId,
    number,
    firstName,
    lastName,
    position,
    minutesPlayed,
    year,
    elig,
    starter,
    fieldGoalsMade,
    fieldGoalsAttempted,
    freeThrowsMade,
    freeThrowsAttempted,
    threePointsMade,
    threePointsAttempted,
    offensiveRebounds,
    totalRebounds,
    assists,
    turnovers,
    personalFouls,
    steals,
    blockedShots,
    points
  )

  box_df
}
