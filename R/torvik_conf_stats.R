#' Get conference-level aggregate stats (Bart Torvik)
#'
#' Aggregates Bart Torvik team ratings and four factors up to the conference
#' level, giving a bird's-eye view of how each conference stacks up in
#' efficiency, power rating, pace, and shooting.
#'
#' @param year Integer. Season year (e.g. `2026` for 2025-26). Must be
#'   `>= 2008`. Defaults to the current year.
#' @param min_teams Integer. Minimum number of teams a conference must have to
#'   be included (filters out tiny or non-standard groupings). Default `4`.
#' @param timeout Numeric. Request timeout in seconds passed to underlying
#'   data functions. Default `20`.
#'
#' @return A tibble with one row per conference, containing:
#'   \describe{
#'     \item{conf}{Conference abbreviation.}
#'     \item{n_teams}{Number of teams in the conference.}
#'     \item{mean_rank}{Average T-Rank ranking across member teams.}
#'     \item{mean_adj_oe}{Average adjusted offensive efficiency.}
#'     \item{mean_adj_de}{Average adjusted defensive efficiency.}
#'     \item{mean_barthag}{Average Barthag power rating.}
#'     \item{mean_adj_tempo}{Average adjusted pace.}
#'     \item{mean_wab}{Average wins above bubble.}
#'     \item{mean_o_efg}{Average offensive eFG\%.}
#'     \item{mean_d_efg}{Average defensive eFG\% allowed.}
#'     \item{mean_o_to_pct}{Average offensive turnover rate.}
#'     \item{mean_d_to_pct}{Average defensive turnover rate forced.}
#'     \item{mean_o_reb_pct}{Average offensive rebound rate.}
#'     \item{mean_d_reb_pct}{Average defensive rebound rate.}
#'     \item{mean_o_ftr}{Average offensive free throw rate.}
#'     \item{mean_d_ftr}{Average defensive free throw rate allowed.}
#'     \item{top_team}{Team with the best Barthag in the conference.}
#'     \item{top_barthag}{Barthag of the top team.}
#'     \item{total_wab}{Sum of WAB across all conference teams.}
#'   }
#'
#' @details
#' This function pulls [torvik_team_ratings()] and [torvik_four_factors()]
#' internally and joins them on team name. If Torvik's naming is slightly
#' inconsistent between endpoints for a small number of teams, those teams may
#' be excluded from the four-factor columns -- the raw counts will still be
#' correct.
#'
#' @examples
#' \dontrun{
#'   conf <- torvik_conf_stats()
#'
#'   # Best offensive conferences
#'   conf |> dplyr::arrange(dplyr::desc(mean_adj_oe))
#'
#'   # Conference defensive rankings
#'   conf |>
#'     dplyr::arrange(mean_adj_de) |>
#'     dplyr::select(conf, n_teams, mean_adj_de, mean_d_efg, mean_d_reb_pct)
#'
#'   # Overall conference strength
#'   conf |>
#'     dplyr::arrange(dplyr::desc(mean_barthag)) |>
#'     dplyr::select(conf, n_teams, mean_barthag, top_team, total_wab)
#' }
#'
#' @seealso [torvik_team_ratings()] for individual team ratings,
#'   [torvik_four_factors()] for team-level four factors.
#'
#' @export
torvik_conf_stats <- function(
    year      = as.integer(format(Sys.Date(), "%Y")),
    min_teams = 4L,
    timeout   = 20
) {

  if (!is.numeric(year) || length(year) != 1 || year < 2008) {
    stop("`year` must be a single numeric value >= 2008.")
  }

  # Pull both source tables
  ratings <- tryCatch(
    torvik_team_ratings(year = year, timeout = timeout),
    error = function(e) stop("Failed to fetch team ratings: ", conditionMessage(e))
  )

  ff <- tryCatch(
    torvik_four_factors(year = year, timeout = timeout),
    error = function(e) {
      warning("Failed to fetch four factors -- conference stats will be partial: ", conditionMessage(e))
      NULL
    }
  )

  # Identify top team per conference from ratings
  top_teams <- ratings |>
    dplyr::filter(!is.na(.data$barthag)) |>
    dplyr::group_by(.data$conf) |>
    dplyr::slice_max(order_by = .data$barthag, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(conf = .data$conf, top_team = .data$team, top_barthag = .data$barthag)

  # Aggregate ratings
  conf_ratings <- ratings |>
    dplyr::group_by(.data$conf) |>
    dplyr::summarise(
      n_teams        = dplyr::n(),
      mean_rank      = mean(.data$rank,      na.rm = TRUE),
      mean_adj_oe    = mean(.data$adj_oe,    na.rm = TRUE),
      mean_adj_de    = mean(.data$adj_de,    na.rm = TRUE),
      mean_barthag   = mean(.data$barthag,   na.rm = TRUE),
      mean_adj_tempo = mean(.data$adj_tempo, na.rm = TRUE),
      mean_wab       = mean(.data$wab,       na.rm = TRUE),
      total_wab      = sum(.data$wab,        na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_teams >= min_teams)

  # Join top team
  conf_ratings <- dplyr::left_join(conf_ratings, top_teams, by = "conf")

  # Aggregate four factors if available
  if (!is.null(ff) && nrow(ff) > 0) {
    conf_ff <- ff |>
      dplyr::group_by(.data$conf) |>
      dplyr::summarise(
        mean_o_efg    = mean(.data$o_efg,    na.rm = TRUE),
        mean_d_efg    = mean(.data$d_efg,    na.rm = TRUE),
        mean_o_to_pct = mean(.data$o_to_pct, na.rm = TRUE),
        mean_d_to_pct = mean(.data$d_to_pct, na.rm = TRUE),
        mean_o_reb_pct = mean(.data$o_reb_pct, na.rm = TRUE),
        mean_d_reb_pct = mean(.data$d_reb_pct, na.rm = TRUE),
        mean_o_ftr    = mean(.data$o_ftr,    na.rm = TRUE),
        mean_d_ftr    = mean(.data$d_ftr,    na.rm = TRUE),
        .groups = "drop"
      )

    conf_ratings <- dplyr::left_join(conf_ratings, conf_ff, by = "conf")
  }

  # Round numeric columns to 2 decimal places for readability
  num_cols <- names(conf_ratings)[vapply(conf_ratings, is.numeric, logical(1))]
  conf_ratings <- dplyr::mutate(
    conf_ratings,
    dplyr::across(dplyr::all_of(num_cols), ~ round(.x, 2))
  )

  dplyr::arrange(conf_ratings, dplyr::desc(.data$mean_barthag))
}
