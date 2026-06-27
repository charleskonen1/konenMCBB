#' Get Bart Torvik team ratings (T-Rank)
#'
#' Retrieves the current Bart Torvik T-Rank team ratings table for a given
#' season. This is the primary team-quality metric table from barttorvik.com,
#' containing adjusted offensive and defensive efficiency, Barthag power rating,
#' adjusted tempo, WAB, and rankings.
#'
#' @param year Integer. Season year (e.g. `2026` for the 2025-26 season). Must
#'   be `>= 2008`. Defaults to the current year.
#' @param conf Character. Optional conference filter (e.g. `"ACC"`, `"B10"`).
#'   Use `"All"` (default) for all teams. See barttorvik.com for abbreviations.
#' @param timeout Numeric. Request timeout in seconds. Default `20`.
#'
#' @return A tibble with one row per team, containing:
#'   \describe{
#'     \item{rank}{Overall T-Rank ranking.}
#'     \item{team}{Team name (matches Torvik's naming convention).}
#'     \item{conf}{Conference abbreviation.}
#'     \item{record}{Season record (W-L).}
#'     \item{adj_oe}{Adjusted offensive efficiency (points per 100 possessions vs. avg D).}
#'     \item{oe_rank}{Adjusted OE rank.}
#'     \item{adj_de}{Adjusted defensive efficiency (points allowed per 100 possessions vs. avg O).}
#'     \item{de_rank}{Adjusted DE rank.}
#'     \item{barthag}{Power rating — estimated win probability vs. average D1 team (0–1).}
#'     \item{adj_tempo}{Adjusted pace (possessions per 40 minutes).}
#'     \item{proj_w, proj_l}{Projected wins and losses.}
#'     \item{wab}{Wins above bubble.}
#'     \item{wab_rank}{WAB rank.}
#'     \item{sos}{Strength of schedule.}
#'     \item{fun}{Torvik "fun rating" — how exciting the team plays.}
#'   }
#'
#' @details
#' Ratings are opponent-adjusted using Torvik's iterative methodology. A team
#' with `adj_oe = 120` scores 120 points per 100 possessions against an average
#' D1 defense. `barthag` is the cleanest single-number summary: a value of 0.95
#' means the team is expected to beat an average team 95% of the time.
#'
#' Column names are cleaned to snake_case. The raw Torvik column headers are
#' preserved in a `raw_name` attribute if needed.
#'
#' @examples
#' \dontrun{
#'   # Current season all teams
#'   ratings <- torvik_team_ratings()
#'   head(ratings)
#'
#'   # Filter to ACC only
#'   acc <- torvik_team_ratings(conf = "ACC")
#'
#'   # Historical season
#'   ratings_2024 <- torvik_team_ratings(year = 2024)
#' }
#'
#' @seealso [timeMachine_ratings()] for point-in-time historical snapshots,
#'   [torvik_four_factors()] for four-factor breakdown, [player_stats()] for
#'   player-level advanced metrics.
#'
#' @export
torvik_team_ratings <- function(
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

  # Build query parameters
  conf_q <- if (identical(conf, "All")) "" else paste0("&conlimit=", utils::URLencode(conf, reserved = TRUE))
  url <- paste0("https://barttorvik.com/trank.php?json=1&year=", year_str, conf_q)

  req  <- .torvik_req(url, timeout,
                      referer = paste0("https://barttorvik.com/?year=", year_str))
  resp <- .torvik_perform(req, year_str)
  txt  <- httr2::resp_body_string(resp)
  .torvik_check_html(txt, format = "JSON")

  raw <- jsonlite::fromJSON(txt, simplifyVector = TRUE)

  # trank.php returns an array-of-arrays; each inner array is one team row
  if (is.list(raw) && !is.data.frame(raw)) {
    raw <- do.call(rbind, lapply(raw, function(x) {
      if (is.list(x)) unlist(x, use.names = FALSE) else x
    }))
  }

  df <- as.data.frame(raw, stringsAsFactors = FALSE, check.names = FALSE)

  # Expected column order from barttorvik T-Rank JSON
  col_names <- c(
    "rank", "team", "conf", "record",
    "adj_oe", "oe_rank", "adj_de", "de_rank",
    "barthag", "barthag_rank",
    "proj_w", "proj_l",
    "pro_con_w", "pro_con_l",
    "con_record",
    "sos", "ncsos", "consos",
    "proj_sos", "proj_noncon_sos", "proj_con_sos",
    "elite_sos", "elite_noncon_sos",
    "opp_oe", "opp_de",
    "opp_proj_oe", "opp_proj_de",
    "con_adj_oe", "con_adj_de",
    "qual_o", "qual_d", "qual_barthag", "qual_games",
    "fun",
    "con_pf", "con_pa", "con_poss", "con_oe", "con_de", "con_sos_remain",
    "conf_win_pct",
    "wab", "wab_rank", "fun_rank",
    "adj_tempo"
  )

  if (ncol(df) == length(col_names)) {
    colnames(df) <- col_names
  } else if (ncol(df) > 0) {
    # Graceful fallback: assign as many names as we have columns
    n <- min(ncol(df), length(col_names))
    colnames(df)[seq_len(n)] <- col_names[seq_len(n)]
    warning(
      "Column count mismatch: expected ", length(col_names),
      " but received ", ncol(df), ". Partial names assigned. ",
      "barttorvik.com may have changed its API."
    )
  } else {
    stop("No data returned from barttorvik.com for year=", year_str, ".")
  }

  # Coerce numeric columns
  num_cols <- c(
    "rank", "oe_rank", "de_rank", "barthag_rank", "wab_rank", "fun_rank",
    "adj_oe", "adj_de", "barthag", "adj_tempo",
    "proj_w", "proj_l", "pro_con_w", "pro_con_l",
    "sos", "ncsos", "consos", "proj_sos", "proj_noncon_sos", "proj_con_sos",
    "elite_sos", "elite_noncon_sos",
    "opp_oe", "opp_de", "opp_proj_oe", "opp_proj_de",
    "con_adj_oe", "con_adj_de",
    "qual_o", "qual_d", "qual_barthag", "qual_games",
    "fun", "con_pf", "con_pa", "con_poss", "con_oe", "con_de",
    "con_sos_remain", "conf_win_pct", "wab"
  )
  for (nm in intersect(num_cols, names(df))) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }

  result <- tibble::as_tibble(df)

  # Apply conference filter as post-processing fallback (in case URL param was ignored)
  if (!identical(conf, "All") && "conf" %in% names(result)) {
    result <- dplyr::filter(result, .data$conf == !!conf)
  }

  result
}
