#' Get team box score game log (Bart Torvik)
#'
#' Retrieves team-level game-by-game box score and efficiency statistics from
#' Bart Torvik's `getgamestats.php` endpoint.
#'
#' @param team Character. Team name as used on barttorvik.com (e.g. `"Duke"`,
#'   `"North Carolina"`). Use `"All"` (default) to return all teams.
#' @param season Numeric. Season year (e.g. `2026` for 2025-26). Default `2026`.
#' @param timeout Numeric. Request timeout in seconds. Default `15`.
#'
#' @return A data.frame with one row per game, containing:
#'   \describe{
#'     \item{date}{Game date.}
#'     \item{AwayTeam, HomeTeam}{Team names.}
#'     \item{Away Fg made, Away Fg Att}{Away field goal totals.}
#'     \item{Away 3pt Fg Made, Away 3pt Fg Att}{Away three-point totals.}
#'     \item{Away FT made, Away FT att}{Away free throw totals.}
#'     \item{Away Orb, Away Drb, Away Rbs}{Away rebound totals.}
#'     \item{Away Asts, Away Steals, Away Blocks, Away Turnovers, Away Fouls}{Away counting stats.}
#'     \item{Away Score, Home Score}{Final scores.}
#'     \item{Home Fg made, ...}{Same set of stats for the home team.}
#'     \item{Pace}{Estimated possessions in the game.}
#'   }
#'
#' @examples
#' \dontrun{
#'   # Duke's full 2025-26 game log
#'   duke <- teamBoxScore(team = "Duke", season = 2026)
#'
#'   # All games, current season
#'   all_games <- teamBoxScore()
#' }
#'
#' @seealso [get_games()] for a cleaner efficiency-focused view per team,
#'   [torvik_four_factors()] for season-aggregate four factors.
#'
#' @export
teamBoxScore <- function(team = "All",
                         season = 2026,
                         timeout = 15) {

  if (!is.character(team) || length(team) != 1) {
    stop("`team` must be a single character string.")
  }

  if (!is.numeric(season) || length(season) != 1) {
    stop("`season` must be a single numeric year.")
  }

  year  <- as.character(season)
  team_q <- utils::URLencode(team, reserved = TRUE)

  base_params <- paste0(
    "sIndex=0",
    "&year=", year,
    "&tvalue=", team_q,
    "&cvalue=All&opcvalue=All&ovalue=All&minwin=All",
    "&mindate=&maxdate=&typev=All&venvalue=All",
    "&minadjo=0&minadjd=200&mintempo=0&minppp=0&minefg=0&mintov=200&minreb=0&minftr=0",
    "&minpppd=200&minefgd=200&mintovd=0&minrebd=200&minftrd=200",
    "&mings=0&mingscript=-100&maxx=100&coach=All&opcoach=All",
    "&adjoSelect=min&adjdSelect=max&tempoSelect=min&pppSelect=min&efgSelect=min",
    "&tovSelect=max&rebSelect=min&ftrSelect=min",
    "&pppdSelect=max&efgdSelect=max&tovdSelect=min&rebdSelect=max&ftrdSelect=max",
    "&gscriptSelect=min&sortToggle=1"
  )

  referer <- paste0("https://www.barttorvik.com/gamestat.php?", base_params)
  xhr_url <- paste0("https://www.barttorvik.com/getgamestats.php?", base_params)

  req <- .torvik_req(xhr_url, timeout, referer = referer)

  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)

  txt <- httr2::resp_body_string(resp)

  x <- jsonlite::fromJSON(
    txt,
    simplifyDataFrame = TRUE,
    simplifyMatrix    = TRUE
  )

  if (is.matrix(x)) x <- as.data.frame(x, stringsAsFactors = FALSE, check.names = FALSE)
  if (!is.data.frame(x)) stop("Unexpected JSON structure: expected data.frame/matrix.")
  if (ncol(x) < 30) stop("Unexpected JSON: fewer than 30 columns returned.")

  v <- x[[30]]
  if (!is.character(v)) stop("Unexpected JSON: column 30 is not character JSON.")

  cols <- c(
    "date", "question", "AwayTeam", "HomeTeam",
    "Away Fg made", "Away Fg Att",
    "Away 3pt Fg Made", "Away 3pt Fg Att",
    "Away FT made", "Away FT att",
    "Away Orb", "Away Drb", "Away Rbs",
    "Away Asts", "Away Steals", "Away Blocks",
    "Away Turnovers", "Away Fouls", "Away Score",
    "Home Fg made", "Home Fg Att",
    "Home 3pt Fg Made", "Home 3pt Fg Att",
    "Home FT made", "Home FT att",
    "Home Orb", "Home Drb", "Home Rbs",
    "Home Asts", "Home Steals", "Home Blocks",
    "Home Turnovers", "Home Fouls", "Home Score",
    "Pace", "backslash", "Repeat1", "Repeat2"
  )

  rows <- lapply(v, function(s) unlist(jsonlite::fromJSON(s), use.names = FALSE))
  widths <- vapply(rows, length, integer(1))
  if (any(widths != length(cols))) {
    bad <- which(widths != length(cols))[1]
    stop(
      "Parsed row length mismatch at row ", bad,
      ". Expected ", length(cols), " fields but got ", widths[bad], "."
    )
  }

  mat <- do.call(rbind, rows)
  df  <- as.data.frame(mat, stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- cols

  dplyr::select(df, -question, -backslash, -Repeat1, -Repeat2)
}
