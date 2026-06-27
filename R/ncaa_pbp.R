#' Get NCAA play-by-play data
#'
#' Retrieves play-by-play data for a given NCAA contest ID from the NCAA's
#' GraphQL API. Returns one row per play event, including clock, score, and
#' event description.
#'
#' @param game_id NCAA contest ID. This is the numeric ID found in NCAA.com
#'   game URLs (e.g. for `https://www.ncaa.com/game/6262407/play-by-play`,
#'   the ID is `"6262407"`). Can be character or numeric.
#'
#' @return A tibble with one row per play event, containing:
#'   \describe{
#'     \item{period}{Period number (1 = first half, 2 = second half, 3+ = OT).}
#'     \item{clock}{Time remaining in the period as a string (e.g. `"14:23"`).}
#'     \item{teamId}{NCAA team identifier for the acting team.}
#'     \item{isHome}{Logical; `TRUE` if the acting team is the home team.}
#'     \item{homeScore}{Home team score after this play.}
#'     \item{visitorScore}{Away team score after this play.}
#'     \item{eventDescription}{Plain-text description of the play.}
#'   }
#'
#' @examples
#' \dontrun{
#'   pbp <- ncaa_pbp("6262407")
#'   head(pbp)
#' }
#'
#' @export
ncaa_pbp <- function(game_id) {

  if (missing(game_id) || length(game_id) != 1) {
    stop("`game_id` must be a single value.")
  }

  game_id <- as.character(game_id)

  query_url <- paste0(
    "https://sdataprod.ncaa.com/",
    "?meta=NCAA_GetGamecenterPbpBasketballById_web",
    "&extensions=%7B%22persistedQuery%22%3A%7B%22version%22%3A1%2C",
    "%22sha256Hash%22%3A%226b1232714a3598954c5bacabc0f81570e16d6ee017c9a6b93b601a3d40dafb98%22%7D%7D",
    "&variables=%7B%22contestId%22%3A%22", game_id,
    "%22%2C%22staticTestEnv%22%3Anull%7D"
  )

  req <- httr2::request(query_url) |>
    httr2::req_headers(
      Accept       = "application/json",
      `User-Agent` = "konenMCBB R package (github.com/charleskonen1/konenMCBB)",
      Referer      = paste0("https://www.ncaa.com/game/", game_id, "/play-by-play")
    ) |>
    httr2::req_timeout(20) |>
    httr2::req_retry(max_tries = 3,
                     is_transient = \(r) httr2::resp_status(r) %in% c(429L, 503L),
                     backoff = \(i) i * 2)

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) stop("Failed to fetch play-by-play from NCAA API: ", conditionMessage(e))
  )
  httr2::resp_check_status(resp)

  txt <- httr2::resp_body_string(resp)
  j   <- tryCatch(
    jsonlite::fromJSON(txt, flatten = TRUE),
    error = function(e) stop("Failed to parse play-by-play JSON: ", conditionMessage(e))
  )

  periods <- j$data$playbyplay$periods

  if (is.null(periods)) {
    stop("No play-by-play data found for game_id: ", game_id)
  }

  periods_df <- tibble::as_tibble(periods)

  pbp_df <- dplyr::select(periods_df, periodNumber, playbyplayStats)
  pbp_df <- tidyr::unnest(pbp_df, cols = playbyplayStats)

  pbp_df <- dplyr::select(
    pbp_df,
    period         = periodNumber,
    clock,
    teamId,
    isHome,
    homeScore,
    visitorScore,
    eventDescription
  )

  pbp_df
}
