#' Display today's Torvik game slate
#'
#' Returns today's unplayed games from the Bart Torvik super schedule, formatted
#' as a clean tibble sorted by game quality (TTQ). Useful for a quick daily
#' preview of what's on the board.
#'
#' @param year Integer. Season year (e.g. `2026`). Defaults to the current
#'   calendar year, which is correct from November onward.
#'
#' @return A tibble with one row per unplayed game today, containing:
#'   \describe{
#'     \item{Matchup}{Team vs. team label from Torvik.}
#'     \item{Line}{Favored team and projected spread.}
#'     \item{Predicted_score}{Torvik's predicted final score.}
#'     \item{WinProb}{Win probability for the favored team.}
#'     \item{TTQ}{Torvik Tier Quality — higher is a better game.}
#'   }
#'   Returns the string `"No games scheduled for today."` if the slate is empty.
#'
#' @examples
#' \dontrun{
#'   dayCast()
#'   dayCast(year = 2025)
#' }
#'
#' @export
dayCast <- function(year = as.integer(format(Sys.Date(), "%Y"))) {

  df <- get_super_sked(year)
  df$Date <- as.Date(df$Date, format = "%m/%d/%y")

  slate <- df |>
    dplyr::filter(.data$Date == Sys.Date(), .data$gp == 0)

  if (nrow(slate) == 0) {
    return("No games scheduled for today.")
  }

  slate |>
    dplyr::transmute(
      Matchup         = .data$matchup,
      Line            = paste0(.data$favored_team, " ", .data$line),
      Predicted_score = .data$predicted_score,
      WinProb         = .data$pctChanceWin,
      TTQ             = round(as.numeric(.data$ttq), 1)
    ) |>
    dplyr::arrange(dplyr::desc(.data$TTQ))
}
