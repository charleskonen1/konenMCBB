#' Get historical Torvik team results
#'
#' Retrieves the full team results table for a given season year.
#'
#' @param year Season year (must be >= 2008)
#'
#' @return A data.frame of team results
#' @export
historic_team_results <- function(
    year = as.integer(format(Sys.Date(), "%Y"))
) {

  if (!is.numeric(year) || year < 2008) {
    stop("`year` must be numeric and >= 2008.")
  }

  year_str <- as.character(year)

  csv_url <- paste0(
    "https://barttorvik.com/",
    year_str,
    "_team_results.csv"
  )

  data <- utils::read.csv(
    csv_url,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  return(data)
}
