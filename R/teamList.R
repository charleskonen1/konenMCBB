#' Get NCAA team list from Sports Reference
#'
#' Retrieves historical NCAA team summary table.
#'
#' @return A data.frame of NCAA schools
#' @export
teamList <- function() {

  url <- "https://www.sports-reference.com/cbb/schools/"

  webpage <- tryCatch(
    rvest::read_html(url),
    error = function(e) stop("Failed to retrieve team list page.")
  )

  node <- rvest::html_element(webpage, "tbody")

  if (is.na(node)) {
    stop("Could not locate table body on page.")
  }

  table <- tryCatch(
    rvest::html_table(node, fill = TRUE),
    error = function(e) stop("Failed to parse team table.")
  )

  df <- as.data.frame(
    table,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  teamCols <- c(
    "Rk","School","City, State",
    "From","To","Yrs","G","W","L",
    "W-L%","SRS","SOS","AP",
    "CREG","CRTN","NCAA","FF","NC"
  )

  if (ncol(df) != length(teamCols)) {
    stop(
      "Column mismatch: expected ", length(teamCols),
      " columns but received ", ncol(df), "."
    )
  }

  colnames(df) <- teamCols

  return(df)
}
