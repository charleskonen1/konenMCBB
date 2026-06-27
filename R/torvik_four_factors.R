#' Get Bart Torvik team four factors
#'
#' Retrieves the four-factors efficiency table from Bart Torvik for a given
#' season. The four factors (eFG%, TO%, Reb%, FT Rate) are the foundational
#' efficiency metrics for both offense and defense, popularized by Dean Oliver.
#'
#' @param year Integer. Season year (e.g. `2026` for the 2025-26 season). Must
#'   be `>= 2008`. Defaults to the current year.
#' @param conf Character. Optional conference filter (e.g. `"ACC"`, `"B10"`).
#'   Use `"All"` (default) for all teams.
#' @param timeout Numeric. Request timeout in seconds. Default `20`.
#'
#' @return A tibble with one row per team, containing:
#'   \describe{
#'     \item{team}{Team name.}
#'     \item{conf}{Conference abbreviation.}
#'     \item{gp}{Games played.}
#'     \item{adj_oe}{Adjusted offensive efficiency.}
#'     \item{adj_de}{Adjusted defensive efficiency.}
#'     \item{barthag}{Power rating.}
#'     \item{o_efg}{Offensive effective field goal percentage.}
#'     \item{o_to_pct}{Offensive turnover percentage (lower is better).}
#'     \item{o_reb_pct}{Offensive rebound percentage.}
#'     \item{o_ftr}{Offensive free throw rate (FTA / FGA).}
#'     \item{d_efg}{Defensive effective field goal percentage allowed.}
#'     \item{d_to_pct}{Defensive turnover percentage forced (higher is better).}
#'     \item{d_reb_pct}{Defensive rebound percentage.}
#'     \item{d_ftr}{Defensive free throw rate allowed.}
#'     \item{adj_tempo}{Adjusted pace (possessions per 40 minutes).}
#'     \item{wab}{Wins above bubble.}
#'   }
#'
#' @details
#' The four factors on offense and defense tell you *why* a team is efficient,
#' not just *that* it is. A team can have a great adjusted OE either because it
#' shoots well (high eFG%), takes care of the ball (low TO%), crashes the glass
#' (high OReb%), or gets to the line (high FTR) — or some combination. This
#' table makes those drivers visible.
#'
#' @examples
#' \dontrun{
#'   # All teams, current season
#'   ff <- torvik_four_factors()
#'
#'   # Big Ten only
#'   b10 <- torvik_four_factors(conf = "B10")
#'
#'   # Teams sorted by defensive eFG% allowed
#'   ff |> dplyr::arrange(d_efg)
#' }
#'
#' @seealso [torvik_team_ratings()] for the full T-Rank table, [get_games()]
#'   for game-level four factors.
#'
#' @export
torvik_four_factors <- function(
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
    "https://barttorvik.com/teamstats.php?csv=1&year=", year_str, conf_q
  )

  req  <- .torvik_req(url, timeout,
                      referer = paste0("https://barttorvik.com/teamstats.php?year=", year_str))
  resp <- .torvik_perform(req, year_str)
  txt  <- httr2::resp_body_string(resp)
  .torvik_check_html(txt, format = "CSV")

  df <- tryCatch(
    utils::read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Failed to parse CSV from barttorvik.com: ", conditionMessage(e))
  )

  if (nrow(df) == 0) {
    stop("No four-factors data returned for year=", year_str, ". Check that the season exists.")
  }

  # Expected columns from teamstats.php CSV (barttorvik.com)
  expected_cols <- c(
    "team", "conf", "gp", "adj_oe", "adj_de", "barthag",
    "o_efg", "o_to_pct", "o_reb_pct", "o_ftr",
    "d_efg", "d_to_pct", "d_reb_pct", "d_ftr",
    "adj_tempo", "wab"
  )

  if (ncol(df) >= length(expected_cols)) {
    colnames(df)[seq_along(expected_cols)] <- expected_cols
  } else {
    n <- ncol(df)
    colnames(df)[seq_len(n)] <- expected_cols[seq_len(n)]
    warning(
      "Column count mismatch: expected >= ", length(expected_cols),
      " but received ", n, ". Partial renaming applied."
    )
  }

  # Numeric coercion
  num_cols <- c(
    "gp", "adj_oe", "adj_de", "barthag",
    "o_efg", "o_to_pct", "o_reb_pct", "o_ftr",
    "d_efg", "d_to_pct", "d_reb_pct", "d_ftr",
    "adj_tempo", "wab"
  )
  for (nm in intersect(num_cols, names(df))) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }

  result <- tibble::as_tibble(df)

  if (!identical(conf, "All") && "conf" %in% names(result)) {
    result <- dplyr::filter(result, .data$conf == !!conf)
  }

  result
}
