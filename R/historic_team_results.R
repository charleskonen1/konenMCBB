#' Get historical Torvik team results
#'
#' Retrieves the full team results table for a given season year from
#' Bart Torvik. Returns one row per team with season-aggregate metrics.
#'
#' @param year Integer. Season year (e.g. `2025` for 2024-25). Must be
#'   `>= 2008`. Defaults to the current calendar year.
#'
#' @return A data.frame of team results for the requested season.
#'
#' @details
#' Data sourced from `barttorvik.com/<year>_team_results.csv`. The file is
#' posted for seasons from 2008 onward. A 404 error means the season file
#' is not available — typically because the season has not started or the
#' data has not been posted to Torvik's server yet.
#'
#' @examples
#' \dontrun{
#'   res <- historic_team_results(2025)
#'   head(res)
#' }
#'
#' @seealso [current_resume()] for the active season, [torvik_team_ratings()]
#'   for the efficiency-focused T-Rank table.
#'
#' @export
historic_team_results <- function(
    year = as.integer(format(Sys.Date(), "%Y"))
) {

  if (!is.numeric(year) || length(year) != 1 || year < 2008) {
    stop("`year` must be a single numeric value >= 2008.")
  }

  year_str <- as.character(as.integer(year))
  csv_url  <- paste0("https://barttorvik.com/", year_str, "_team_results.csv")

  req  <- .torvik_req(csv_url, timeout = 30,
                      referer = "https://barttorvik.com/")
  resp <- .torvik_perform(req, year_str)
  txt  <- httr2::resp_body_string(resp)
  .torvik_check_html(txt, format = "CSV")

  data <- tryCatch(
    utils::read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) stop("Failed to parse team results CSV: ", conditionMessage(e))
  )

  if (nrow(data) == 0) {
    stop(
      "No team results returned for year = ", year_str, ". ",
      "The file may not be posted yet for this season.",
      call. = FALSE
    )
  }

  data
}
