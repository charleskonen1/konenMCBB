#' Get current Torvik team resume table
#'
#' Returns the most current Bart Torvik team results table as a tibble.
#'
#' @return A tibble containing team metrics.
#' @export
current_resume <- function() {

  month <- as.integer(format(Sys.Date(), "%m"))

  if (month >= 11) {
    year <- as.integer(format(Sys.Date(), "%Y")) + 1
  } else {
    year <- as.integer(format(Sys.Date(), "%Y"))
  }

  year_str <- as.character(year)
  csv_url <- paste0("https://barttorvik.com/", year_str, "_team_results.csv")

  data <- utils::read.csv(csv_url)

  return(tibble::as_tibble(data))
}
