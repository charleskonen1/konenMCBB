#' Get current Torvik team resume table
#'
#' Returns the most current Bart Torvik team results table as a tibble.
#' Automatically selects the correct season year: November–December uses
#' `current_year + 1` (new season); all other months use `current_year`.
#'
#' @return A tibble containing Torvik team metrics for the active season,
#'   or the most recently completed season if called in the off-season.
#'
#' @details
#' Data sourced from `barttorvik.com/<year>_team_results.csv`. This file
#' is available during and shortly after the active season. If it returns
#' 404 the data is not yet posted; try `historic_team_results()` with an
#' explicit previous year.
#'
#' @examples
#' \dontrun{
#'   resume <- current_resume()
#'   head(resume)
#' }
#'
#' @seealso [historic_team_results()] for an explicit year, [torvik_team_ratings()]
#'   for the live T-Rank efficiency table.
#'
#' @export
current_resume <- function() {

  month <- as.integer(format(Sys.Date(), "%m"))
  year  <- if (month >= 11L) {
    as.integer(format(Sys.Date(), "%Y")) + 1L
  } else {
    as.integer(format(Sys.Date(), "%Y"))
  }

  year_str <- as.character(year)
  csv_url  <- paste0("https://barttorvik.com/", year_str, "_team_results.csv")

  req  <- .torvik_req(csv_url, timeout = 20,
                      referer = "https://barttorvik.com/")
  resp <- .torvik_perform(req, year_str)
  txt  <- httr2::resp_body_string(resp)
  .torvik_check_html(txt, format = "CSV")

  data <- tryCatch(
    utils::read.csv(text = txt, stringsAsFactors = FALSE),
    error = function(e) stop("Failed to parse team results CSV: ", conditionMessage(e))
  )

  if (nrow(data) == 0) {
    stop(
      "No team results returned for year = ", year_str, ". ",
      "The file may not be posted yet for this season.",
      call. = FALSE
    )
  }

  tibble::as_tibble(data)
}
