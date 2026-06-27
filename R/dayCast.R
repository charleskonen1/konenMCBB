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
    dplyr::mutate(
      t1wp_n  = suppressWarnings(as.numeric(.data$t1wp)),
      t2wp_n  = suppressWarnings(as.numeric(.data$t2wp)),
      Favorite = dplyr::if_else(
        is.na(.data$t1wp_n) | .data$t1wp_n >= .data$t2wp_n,
        .data$team1, .data$team2
      ),
      WinProb = dplyr::if_else(
        is.na(.data$t1wp_n) | .data$t1wp_n >= .data$t2wp_n,
        round(.data$t1wp_n, 1), round(.data$t2wp_n, 1)
      ),
      Pred_Score = paste0(
        suppressWarnings(round(as.numeric(.data$t1ppp), 0)), "-",
        suppressWarnings(round(as.numeric(.data$t2ppp), 0))
      )
    ) |>
    dplyr::transmute(
      Matchup    = .data$matchup,
      Favorite   = .data$Favorite,
      Pred_Score = .data$Pred_Score,
      WinProb    = .data$WinProb,
      Line       = .data$prediction,
      TTQ        = round(suppressWarnings(as.numeric(.data$ttq)), 1)
    ) |>
    dplyr::arrange(dplyr::desc(.data$TTQ))
}
