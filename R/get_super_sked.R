#' Get Bart Torvik super schedule
#'
#' Retrieves the full Torvik super schedule table for a given season.
#' Includes predicted scores, win probabilities, game quality (TTQ), and
#' actual results once games have been played.
#'
#' @param year Integer. Season year (e.g. `2026` for the 2025-26 season).
#'   Must be `>= 2008`. Defaults to the current calendar year.
#'
#' @return A data.frame with one row per scheduled game, containing columns
#'   such as `matchup`, `team1`, `team2`, `t1wp`, `t2wp`, `t1ppp`, `t2ppp`,
#'   `prediction`, `ttq`, `gp` (games played flag), `Date`, and more.
#'   Also adds parsed columns `favored_team`, `line`, `predicted_score`,
#'   `pctChanceWin` when `prediction` can be parsed.
#'
#' @details
#' Data is sourced from `barttorvik.com/<year>_super_sked.csv`. The file
#' is only available during and shortly after the active season
#' (roughly November–April). Requesting a year with no data on the server
#' will raise an informative error suggesting you try a previous year.
#'
#' @examples
#' \dontrun{
#'   sked <- get_super_sked(2025)
#'   head(sked)
#' }
#'
#' @seealso [dayCast()] for a filtered, formatted today-only view.
#'
#' @export
get_super_sked <- function(
    year = as.integer(format(Sys.Date(), "%Y"))
) {

  if (!is.numeric(year) || length(year) != 1 || year < 2008) {
    stop("`year` must be a single numeric value >= 2008.")
  }

  year_str <- as.character(as.integer(year))
  csv_url  <- paste0("https://barttorvik.com/", year_str, "_super_sked.csv")

  req  <- .torvik_req(csv_url, timeout = 30,
                      referer = paste0("https://barttorvik.com/?year=", year_str))
  resp <- .torvik_perform(req, year_str)
  txt  <- httr2::resp_body_string(resp)
  .torvik_check_html(txt, format = "CSV")

  data <- tryCatch(
    utils::read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Failed to parse super-schedule CSV: ", conditionMessage(e))
  )

  if (nrow(data) == 0) {
    stop(
      "No schedule data returned for year = ", year_str, ". ",
      "The season file may not be available yet.",
      call. = FALSE
    )
  }

  expected_cols <- c(
    "muid", "Date", "confmatch", "matchup", "prediction",
    "ttq", "conf", "venue",
    "team1", "t1oe", "t1de", "t1py", "t1wp", "t1propt",
    "team2", "t2oe", "t2de", "t2py", "t2wp", "t2propt",
    "tpro", "t1qual", "t2qual", "gp", "result", "tempo",
    "possessions", "t1pts", "t2pts", "winner", "loser",
    "t1adjt", "t2adjt", "t1adjo", "t1adjd", "t2adjo", "t2adjd",
    "gamevalue", "mistmach", "blowout", "t1elite", "t2elite",
    "ord_date", "t1ppp", "t2ppp", "gameppp",
    "t1rk", "t2rk", "t1gs", "t2gs", "gamestats",
    "overtimes", "t1fun", "t2fun", "results"
  )

  n_assign <- min(ncol(data), length(expected_cols))
  colnames(data)[seq_len(n_assign)] <- expected_cols[seq_len(n_assign)]

  if (ncol(data) != length(expected_cols)) {
    warning(
      "Column count mismatch in super-schedule: expected ", length(expected_cols),
      " but received ", ncol(data), ". ",
      "barttorvik.com may have updated the file format. ",
      "Columns renamed up to column ", n_assign, ".",
      call. = FALSE
    )
  }

  # Parse the prediction string into helper columns when possible
  if ("prediction" %in% names(data) &&
      any(grepl(", ", data$prediction, fixed = TRUE))) {
    split_pred <- tryCatch(
      stringr::str_split(data$prediction, ", ", simplify = TRUE),
      error = function(e) NULL
    )
    if (!is.null(split_pred) && ncol(split_pred) >= 2) {
      col1 <- stringr::str_split(split_pred[, 1], "-", simplify = TRUE)
      col2 <- stringr::str_split(split_pred[, 2], " ", simplify = TRUE)
      data$predicted_score <- if (ncol(col2) >= 1) col2[, 1] else NA_character_
      data$pctChanceWin    <- if (ncol(col2) >= 2) col2[, 2] else NA_character_
      data$favored_team    <- if (ncol(col1) >= 1) col1[, 1] else NA_character_
      data$line            <- if (ncol(col1) >= 2) col1[, 2] else NA_character_
    }
  } else {
    data$predicted_score <- NA_character_
    data$pctChanceWin    <- NA_character_
    data$favored_team    <- NA_character_
    data$line            <- NA_character_
  }

  data
}
