#' Get Bart Torvik team shooting splits
#'
#' Retrieves shot location efficiency data for every team from Bart Torvik —
#' how often teams shoot from the rim, mid-range, and three-point line, and
#' how efficiently they convert from each zone, on both offense and defense.
#'
#' @param year Integer. Season year (e.g. `2026` for 2025-26). Must be
#'   `>= 2008`. Defaults to the current year.
#' @param conf Character. Optional conference filter (e.g. `"ACC"`, `"B10"`).
#'   Use `"All"` (default) for all teams.
#' @param timeout Numeric. Request timeout in seconds. Default `20`.
#'
#' @return A tibble with one row per team, containing:
#'   \describe{
#'     \item{team}{Team name (Torvik convention).}
#'     \item{conf}{Conference abbreviation.}
#'     \item{gp}{Games played.}
#'
#'     **Offensive shooting splits:**
#'     \item{o_rim_made, o_rim_att}{Rim shot makes and attempts (offense).}
#'     \item{o_rim_pct}{Offensive rim shooting percentage.}
#'     \item{o_rim_rate}{Percentage of field goal attempts taken at the rim.}
#'     \item{o_mid_made, o_mid_att}{Mid-range makes and attempts (offense).}
#'     \item{o_mid_pct}{Offensive mid-range shooting percentage.}
#'     \item{o_mid_rate}{Percentage of field goal attempts from mid-range.}
#'     \item{o_three_made, o_three_att}{Three-point makes and attempts (offense).}
#'     \item{o_three_pct}{Offensive three-point percentage.}
#'     \item{o_three_rate}{Percentage of field goal attempts from three.}
#'     \item{o_dunk_made, o_dunk_att}{Dunk makes and attempts (offense).}
#'     \item{o_dunk_pct}{Offensive dunk percentage.}
#'
#'     **Defensive shooting splits (opponent shots against this team):**
#'     \item{d_rim_made, d_rim_att}{Rim shot makes and attempts allowed.}
#'     \item{d_rim_pct}{Defensive rim shooting percentage allowed.}
#'     \item{d_rim_rate}{Percentage of opponent FGA at the rim.}
#'     \item{d_mid_made, d_mid_att}{Mid-range makes and attempts allowed.}
#'     \item{d_mid_pct}{Defensive mid-range percentage allowed.}
#'     \item{d_mid_rate}{Percentage of opponent FGA from mid-range.}
#'     \item{d_three_made, d_three_att}{Three-point makes and attempts allowed.}
#'     \item{d_three_pct}{Defensive three-point percentage allowed.}
#'     \item{d_three_rate}{Percentage of opponent FGA from three.}
#'     \item{d_dunk_made, d_dunk_att}{Dunk makes and attempts allowed.}
#'     \item{d_dunk_pct}{Defensive dunk percentage allowed.}
#'   }
#'
#' @details
#' Shot location data is one of the most actionable layers of college basketball
#' analytics. Understanding *where* a team gets its shots (and forces opponents
#' to shoot) goes beyond the four factors. A team with a high `o_rim_rate` and
#' high `o_rim_pct` is generating high-quality offense near the basket; a team
#' with low `d_three_rate` is successfully pushing opponents away from the arc.
#'
#' "Rim" shots are typically defined as within 4 feet of the basket.
#' "Mid-range" covers everything between the rim zone and the three-point line.
#'
#' @examples
#' \dontrun{
#'   # All teams, current season
#'   shooting <- torvik_shooting()
#'
#'   # Best rim-finishing teams
#'   shooting |>
#'     dplyr::arrange(dplyr::desc(o_rim_pct)) |>
#'     dplyr::select(team, conf, o_rim_pct, o_rim_rate, o_three_rate)
#'
#'   # Best rim-protecting defenses
#'   shooting |>
#'     dplyr::arrange(d_rim_pct) |>
#'     dplyr::select(team, conf, d_rim_pct, d_rim_rate)
#'
#'   # Teams that live at the rim (high rim rate) and shoot few mid-rangers
#'   shooting |>
#'     dplyr::filter(o_rim_rate > 0.40, o_mid_rate < 0.20) |>
#'     dplyr::select(team, conf, o_rim_rate, o_mid_rate, o_three_rate, o_rim_pct)
#'
#'   # SEC teams
#'   torvik_shooting(conf = "SEC")
#' }
#'
#' @seealso [torvik_four_factors()] for eFG%, TO%, Reb%, FT Rate;
#'   [torvik_team_ratings()] for adjusted efficiency and power ratings.
#'
#' @export
torvik_shooting <- function(
    year    = as.integer(format(Sys.Date(), "%Y")),
    conf    = "All",
    timeout = 20
) {

  if (!is.numeric(year) || length(year) != 1 || year < 2008) {
    stop("`year` must be a single numeric value >= 2008.")
  }
  if (!is.character(conf) || length(conf) != 1) {
    stop("`conf` must be a single character string.")
  }

  year_str <- as.character(as.integer(year))
  conf_q   <- if (identical(conf, "All")) "" else paste0("&conlimit=", utils::URLencode(conf, reserved = TRUE))

  url <- paste0(
    "https://barttorvik.com/shooting.php?csv=1&year=", year_str, conf_q
  )

  req <- .torvik_req(url, timeout,
                     referer = paste0("https://barttorvik.com/shooting.php?year=", year_str))

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) stop("Failed to reach barttorvik.com: ", conditionMessage(e))
  )
  httr2::resp_check_status(resp)

  txt <- httr2::resp_body_string(resp)

  df <- tryCatch(
    utils::read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Failed to parse shooting CSV: ", conditionMessage(e))
  )

  if (nrow(df) == 0) {
    stop("No shooting data returned for year=", year_str, ".")
  }

  # Expected column structure from barttorvik shooting.php CSV
  expected_cols <- c(
    "team", "conf", "gp",
    # Offense
    "o_rim_made", "o_rim_att", "o_rim_pct", "o_rim_rate",
    "o_mid_made", "o_mid_att", "o_mid_pct", "o_mid_rate",
    "o_three_made", "o_three_att", "o_three_pct", "o_three_rate",
    "o_dunk_made", "o_dunk_att", "o_dunk_pct",
    # Defense
    "d_rim_made", "d_rim_att", "d_rim_pct", "d_rim_rate",
    "d_mid_made", "d_mid_att", "d_mid_pct", "d_mid_rate",
    "d_three_made", "d_three_att", "d_three_pct", "d_three_rate",
    "d_dunk_made", "d_dunk_att", "d_dunk_pct"
  )

  n_assign <- min(ncol(df), length(expected_cols))
  colnames(df)[seq_len(n_assign)] <- expected_cols[seq_len(n_assign)]

  if (ncol(df) != length(expected_cols)) {
    warning(
      "Column count mismatch: expected ", length(expected_cols),
      " but received ", ncol(df), ". ",
      "barttorvik.com may have updated the shooting endpoint. ",
      "Columns assigned up to column ", n_assign, "."
    )
  }

  # Numeric coercion
  num_cols <- setdiff(expected_cols, c("team", "conf"))
  for (nm in intersect(num_cols, names(df))) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }

  result <- tibble::as_tibble(df)

  # Post-process conference filter as fallback
  if (!identical(conf, "All") && "conf" %in% names(result)) {
    result <- dplyr::filter(result, .data$conf == !!conf)
  }

  result
}
