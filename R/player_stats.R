#' Get Bart Torvik advanced player stats
#'
#' Retrieves the full advanced player statistics table from Bart Torvik for a
#' given season. Includes efficiency, usage, shot location, BPM, and recruiting
#' data for all D1 players.
#'
#' @param year Numeric. Season year (e.g. `2026` for the 2025-26 season). Must
#'   be `>= 2008`. Defaults to the current calendar year.
#'
#' @return A tibble with one row per player, containing:
#'   \describe{
#'     \item{player_name}{Player name.}
#'     \item{team}{Team name (Torvik convention).}
#'     \item{conf}{Conference abbreviation.}
#'     \item{GP}{Games played.}
#'     \item{Min_pct}{Percentage of team minutes played.}
#'     \item{ORtg}{Offensive rating (points produced per 100 possessions used).}
#'     \item{usg}{Usage rate — percentage of team possessions used.}
#'     \item{eFG}{Effective field goal percentage.}
#'     \item{TS_pct}{True shooting percentage.}
#'     \item{ORB_pct, DRB_pct}{Offensive and defensive rebound percentages.}
#'     \item{AST_pct}{Assist percentage.}
#'     \item{TO_pct}{Turnover percentage.}
#'     \item{FTM, FTA, FT_pct}{Free throw totals and percentage.}
#'     \item{twoPM, twoPA, twoP_pct}{Two-point totals.}
#'     \item{TPM, TPA, TP_pct}{Three-point totals.}
#'     \item{blk_pct, stl_pct, ftr_pct}{Block, steal, and free throw rate percentages.}
#'     \item{yr}{Academic year (Fr, So, Jr, Sr, Gr).}
#'     \item{ht}{Height.}
#'     \item{porpag}{Points over replacement per adjusted game.}
#'     \item{adjoe}{Adjusted offensive efficiency for the player's team.}
#'     \item{bpm, obpm, dbpm, gbpm}{Box plus/minus variants.}
#'     \item{rimmade, rimmade_rimmiss}{Rim shooting totals.}
#'     \item{midmade, midmade_midmiss}{Mid-range shooting totals.}
#'     \item{dunksmade, dunksmiss_dunksmade}{Dunk totals.}
#'     \item{oreb, dreb, treb, ast, stl, blk, pts}{Season counting stats.}
#'     \item{role}{Torvik role classification.}
#'   }
#'
#' @details
#' BPM variants: `bpm` is total box plus/minus; `obpm` is offensive BPM;
#' `dbpm` is defensive BPM; `gbpm` is the "grades" BPM variant used for
#' player-to-player comparisons.
#'
#' @examples
#' \dontrun{
#'   # Current season
#'   players <- player_stats()
#'
#'   # Top offensive players by ORtg (min 20% usage)
#'   players |>
#'     dplyr::filter(usg >= 20) |>
#'     dplyr::arrange(dplyr::desc(ORtg)) |>
#'     dplyr::select(player_name, team, ORtg, usg, eFG, TS_pct)
#'
#'   # 2024 season
#'   p2024 <- player_stats(year = 2024)
#' }
#'
#' @seealso [torvik_team_ratings()] for team-level aggregates,
#'   [PlayerBoxScore()] for game-by-game logs from Sports Reference.
#'
#' @export
player_stats <- function(
    year = as.integer(format(Sys.Date(), "%Y"))
) {

  if (!is.numeric(year) || length(year) != 1 || year < 2008) {
    stop("`year` must be numeric and >= 2008.")
  }

  year_str <- as.character(as.integer(year))

  # Note: barttorvik uses an unconventional query string here (?YEAR&csv=1)
  # where the year is an unnamed positional parameter.
  csv_url <- paste0(
    "https://barttorvik.com/getadvstats.php?", year_str, "&csv=1"
  )

  req  <- .torvik_req(csv_url, timeout = 45,
                      referer = paste0("https://barttorvik.com/playerstat.php?year=", year_str))
  resp <- .torvik_perform(req, year_str)
  txt  <- httr2::resp_body_string(resp)
  .torvik_check_html(txt, format = "CSV")

  data <- tryCatch(
    utils::read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Failed to parse player stats CSV: ", conditionMessage(e))
  )

  if (nrow(data) == 0) {
    stop(
      "No player data returned for year = ", year_str, ". ",
      "The season data may not be available yet.",
      call. = FALSE
    )
  }

  expected_cols <- c(
    "player_name", "team", "conf", "GP", "Min_pct",
    "ORtg", "usg", "eFG", "TS_pct", "ORB_pct", "DRB_pct", "AST_pct", "TO_pct",
    "FTM", "FTA", "FT_pct", "twoPM", "twoPA", "twoP_pct", "TPM",
    "TPA", "TP_pct", "blk_pct", "stl_pct", "ftr_pct", "yr", "ht", "num",
    "porpag", "adjoe", "pfr", "year", "pid", "Hometown",
    "Recruit_TRank",
    "ast_tov", "rimmade", "rimmade_rimmiss",
    "midmade", "midmade_midmiss",
    "rim_pct",
    "mid_pct",
    "dunksmade", "dunksmiss_dunksmade",
    "dunk_pct",
    "pick", "drtg", "adrtg", "dporpag", "stops",
    "bpm", "obpm", "dbpm", "gbpm", "mp", "ogbpm",
    "dgbpm", "oreb", "dreb", "treb", "ast",
    "stl", "blk", "pts", "role", "help", "birthday"
  )

  n_assign <- min(ncol(data), length(expected_cols))
  colnames(data)[seq_len(n_assign)] <- expected_cols[seq_len(n_assign)]

  if (ncol(data) != length(expected_cols)) {
    warning(
      "Column count mismatch in player stats: expected ", length(expected_cols),
      " but received ", ncol(data), ". ",
      "barttorvik.com may have updated the player stats endpoint. ",
      "Columns assigned up to column ", n_assign, ".",
      call. = FALSE
    )
  }

  tibble::as_tibble(data)
}
