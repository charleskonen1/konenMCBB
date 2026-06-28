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

  # The live teamstats.php endpoint is Cloudflare-gated. The static
  # `<year>_fffinal.csv` file carries the four factors plus shooting splits
  # (3P%, 2P%, FT%, 3P rate, assist rate) for offense and defense, and is
  # not gated. It lacks conf / adj efficiency / barthag, so we join those
  # in from `<year>_team_results.csv` (via torvik_team_ratings()).
  url <- paste0("https://barttorvik.com/", year_str, "_fffinal.csv")

  req  <- .torvik_req(url, timeout,
                      referer = paste0("https://barttorvik.com/?year=", year_str))
  resp <- .torvik_perform(req, year_str)
  txt  <- httr2::resp_body_string(resp)
  .torvik_check_html(txt, format = "CSV")

  # The published header row names only 37 fields but data rows carry 41
  # (Torvik added two trailing stat pairs not reflected in the header), so we
  # skip the header and assign our own names.
  df <- tryCatch(
    utils::read.csv(text = txt, header = FALSE, skip = 1,
                    stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Failed to parse four-factors CSV: ", conditionMessage(e))
  )

  if (nrow(df) == 0) {
    stop("No four-factors data returned for year=", year_str, ". Check that the season exists.")
  }

  # fffinal.csv: TeamName then interleaved value/Rk pairs.
  fffinal_names <- c(
    "team",
    "o_efg",  "rk1",  "d_efg",   "rk2",
    "o_ftr",  "rk3",  "d_ftr",   "rk4",
    "o_reb_pct", "rk5", "d_reb_pct", "rk6",
    "o_to_pct",  "rk7", "d_to_pct",  "rk8",
    "o_three_pct", "rk9",  "d_three_pct", "rk10",
    "o_two_pct",   "rk11", "d_two_pct",   "rk12",
    "o_ft_pct",    "rk13", "d_ft_pct",    "rk14",
    "o_three_rate","rk15", "d_three_rate","rk16",
    "o_ast_rate",  "rk17", "d_ast_rate",  "rk18",
    "o_extra",     "rk19", "d_extra",     "rk20"
  )
  n_assign <- min(ncol(df), length(fffinal_names))
  colnames(df)[seq_len(n_assign)] <- fffinal_names[seq_len(n_assign)]
  df <- df[, setdiff(names(df), grep("^rk[0-9]+$|extra", names(df), value = TRUE)), drop = FALSE]

  # Numeric coercion for everything except team name
  for (nm in setdiff(names(df), "team")) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }

  # Join conf, adj efficiency, barthag, tempo, wab, gp from team ratings
  ratings <- tryCatch(
    torvik_team_ratings(year = year, conf = "All", timeout = timeout),
    error = function(e) NULL
  )
  if (!is.null(ratings) && nrow(ratings) > 0) {
    keep <- intersect(c("team","conf","adj_oe","adj_de","barthag","adj_tempo","wab"), names(ratings))
    df <- dplyr::left_join(df, ratings[, keep], by = "team")
  } else {
    for (col in c("conf","adj_oe","adj_de","barthag","adj_tempo","wab")) {
      if (!col %in% names(df)) df[[col]] <- NA
    }
  }

  result <- tibble::as_tibble(df)

  if (!identical(conf, "All") && "conf" %in% names(result)) {
    result <- dplyr::filter(result, .data$conf == !!conf)
  }

  result
}
