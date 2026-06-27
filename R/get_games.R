#' Get Bart Torvik game-level efficiency stats
#'
#' Retrieves game-by-game efficiency statistics for a given team and season
#' from Bart Torvik's `getgamestats.php` endpoint. Each row is one game, with
#' both raw and adjusted four-factor-style efficiency numbers.
#'
#' @param team Character. Team name as used on barttorvik.com (e.g. `"Duke"`,
#'   `"Gonzaga"`). Use `"All"` (default) to return all teams.
#' @param season Numeric or character. Season year (e.g. `2026` for 2025-26).
#'   Defaults to the current year.
#' @param quad Character or integer. Quadrant filter. One of `1`, `2`, `3`,
#'   `4`, or `"All"` (default). Quadrant is based on location-adjusted opponent
#'   ranking.
#'
#' @return A cleaned data.frame with one row per game, containing:
#'   \describe{
#'     \item{Date}{Game date.}
#'     \item{Team, Opp}{Team and opponent names.}
#'     \item{Conf, Opp_Conf}{Conference labels.}
#'     \item{Venue}{Game venue (`H`, `A`, or `N`).}
#'     \item{Result}{Game result (`W` or `L`).}
#'     \item{Adj. O, Adj. D}{Adjusted offensive and defensive efficiency for the game.}
#'     \item{O_EFF, O_eFG\%, O_TO\%, O_Reb\%, O_FTR}{Offensive four factors.}
#'     \item{D_EFF, D_eFG\%, D_TO\%, D_Reb\%, D_FTR}{Defensive four factors.}
#'     \item{GameScore}{Torvik game quality score.}
#'     \item{Tempo}{Possessions per 40 minutes.}
#'     \item{gameID}{Torvik game identifier (useful for joins).}
#'     \item{Coach, OppCoach}{Coaching staff.}
#'     \item{AvgScoreDiff}{Average score differential vs. opponent's avg.}
#'     \item{Opp_Barthag}{Opponent's Barthag power rating.}
#'   }
#'
#' @examples
#' \dontrun{
#'   # Duke's full 2025-26 game log
#'   duke <- get_games(team = "Duke", season = 2026)
#'
#'   # All Quadrant 1 games in 2025-26
#'   q1 <- get_games(quad = 1)
#'
#'   # Filter to wins
#'   duke |> dplyr::filter(Result == "W")
#' }
#'
#' @seealso [teamBoxScore()] for raw counting stats per game, [get_super_sked()]
#'   for the full schedule with predictions.
#'
#' @export
get_games <- function(
    team   = "All",
    season = as.integer(format(Sys.Date(), "%Y")),
    quad   = "All"
) {

  timeout <- 15

  quadToggle <- ""
  if (!identical(quad, "All")) {
    if (quad %in% 1:4) {
      quadToggle <- paste0("&=", quad)
    } else {
      stop("`quad` must be 1, 2, 3, 4, or 'All'.")
    }
  }

  year <- as.character(season)

  base_query <- paste0(
    "sIndex=0&year=", year,
    "&tvalue=", team,
    "&cvalue=All&opcvalue=All&ovalue=All&minwin=All",
    "&mindate=&maxdate=&typev=All&venvalue=All",
    "&minadjo=0&minadjd=200&mintempo=0&minppp=0",
    "&minefg=0&mintov=200&minreb=0&minftr=0",
    "&minpppd=200&minefgd=200&mintovd=0",
    "&minrebd=200&minftrd=200&mings=0",
    "&mingscript=-100&maxx=100&coach=All&opcoach=All",
    "&adjoSelect=min&adjdSelect=max",
    "&tempoSelect=min&pppSelect=min",
    "&efgSelect=min&tovSelect=max",
    "&rebSelect=min&ftrSelect=min",
    "&pppdSelect=max&efgdSelect=max",
    "&tovdSelect=min&rebdSelect=max",
    "&ftrdSelect=max&gscriptSelect=min",
    "&sortToggle=1",
    quadToggle
  )

  referer <- paste0("https://www.barttorvik.com/gamestat.php?", base_query)
  xhr_url <- paste0("https://www.barttorvik.com/getgamestats.php?", base_query)

  req <- .torvik_req(xhr_url, timeout, referer = referer)

  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)

  txt <- httr2::resp_body_string(resp)

  x <- jsonlite::fromJSON(txt, simplifyMatrix = TRUE)

  df <- if (is.matrix(x)) {
    as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (is.data.frame(x)) {
    as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (is.list(x)) {
    as.data.frame(do.call(rbind, x), stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop("Unexpected JSON structure returned from endpoint.")
  }

  cols <- c(
    "Date", "Type", "Team", "Conf", "Opp", "Venue", "Result",
    "Adj. O", "Adj. D",
    "O_EFF", "O_eFG%", "O_TO%", "O_Reb%", "O_FTR",
    "D_EFF", "D_eFG%", "D_TO%", "D_Reb%", "D_FTR",
    "GameScore", "Opp_Conf", "IDOrder", "Season",
    "Tempo", "gameID", "Coach", "OppCoach",
    "AvgScoreDiff", "Opp_Barthag", "boxScore", "question"
  )

  if (ncol(df) != length(cols)) {
    stop(
      "Column mismatch: endpoint returned ", ncol(df),
      " columns, but expected ", length(cols), "."
    )
  }

  colnames(df) <- cols

  dplyr::select(df, -question, -boxScore, -Type)
}
