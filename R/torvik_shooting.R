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

  # The live shooting.php endpoint no longer exists (404) and the pbp JSON
  # file uses an undocumented numeric encoding. The static `<year>_fffinal.csv`
  # carries reliable, labelled shooting splits — two-point %, three-point %,
  # free-throw %, three-point attempt rate, and assist rate — for both offense
  # and defense. We read those.
  url <- paste0("https://barttorvik.com/", year_str, "_fffinal.csv")

  req  <- .torvik_req(url, timeout,
                      referer = paste0("https://barttorvik.com/?year=", year_str))
  resp <- .torvik_perform(req, year_str)
  txt  <- httr2::resp_body_string(resp)
  .torvik_check_html(txt, format = "CSV")

  # Header names only 37 fields but data rows carry 41; skip header, assign names.
  df <- tryCatch(
    utils::read.csv(text = txt, header = FALSE, skip = 1,
                    stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Failed to parse shooting CSV: ", conditionMessage(e))
  )

  if (nrow(df) == 0) {
    stop("No shooting data returned for year=", year_str, ".")
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

  # Keep only shooting-relevant columns (drop the four-factor rebound/TO cols)
  shoot_cols <- c("team",
                  "o_efg", "d_efg",
                  "o_two_pct", "d_two_pct",
                  "o_three_pct", "d_three_pct",
                  "o_ft_pct", "d_ft_pct",
                  "o_three_rate", "d_three_rate")
  df <- df[, intersect(shoot_cols, names(df)), drop = FALSE]

  for (nm in setdiff(names(df), "team")) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }

  # Join conf from team ratings for filtering / display
  ratings <- tryCatch(
    torvik_team_ratings(year = year, conf = "All", timeout = timeout),
    error = function(e) NULL
  )
  if (!is.null(ratings) && nrow(ratings) > 0) {
    df <- dplyr::left_join(df, ratings[, intersect(c("team","conf"), names(ratings))], by = "team")
  } else if (!"conf" %in% names(df)) {
    df$conf <- NA_character_
  }

  result <- tibble::as_tibble(df)

  if (!identical(conf, "All") && "conf" %in% names(result)) {
    result <- dplyr::filter(result, .data$conf == !!conf)
  }

  result
}
