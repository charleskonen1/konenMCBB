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

  resp <- tryCatch(
    httr2::req_perform(.torvik_req(csv_url, timeout = 30)),
    error = function(e) stop("Failed to reach barttorvik.com: ", conditionMessage(e))
  )
  httr2::resp_check_status(resp)
  txt <- httr2::resp_body_string(resp)
  data <- utils::read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE)

  return(data)
}
