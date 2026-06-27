#' Get team roster from Sports Reference
#'
#' Retrieves team roster data for a given school and season.
#'
#' @param team Team name (e.g., "Duke")
#' @param season Season year (numeric)
#'
#' @return A data.frame containing roster information
#' @export
teamRoster <- function(team, season) {

  if (!is.character(team) || length(team) != 1) {
    stop("`team` must be a single character string.")
  }

  if (!is.numeric(season) || length(season) != 1) {
    stop("`season` must be a single numeric year.")
  }

  team_str <- gsub(" ", "-", team)
  lowercase_team <- tolower(team_str)
  year_str <- as.character(season)

  url <- paste0(
    "https://www.sports-reference.com/cbb/schools/",
    lowercase_team,
    "/men/",
    year_str,
    ".html"
  )

  webpage <- tryCatch(
    rvest::read_html(url),
    error = function(e) stop("Failed to retrieve roster page.")
  )

  node <- rvest::html_element(webpage, "tbody")

  if (is.na(node)) {
    stop("Roster table not found on page.")
  }

  table <- tryCatch(
    rvest::html_table(node, fill = TRUE),
    error = function(e) stop("Failed to parse roster table.")
  )

  if (is.null(table)) {
    stop("Roster table extraction returned NULL.")
  }

  df_box <- as.data.frame(
    table,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  playerBoxCols <- c(
    "Player","#","Class","Pos",
    "Height","Weight","Hometown",
    "High School","RSCI Top 100","Summary"
  )

  if (ncol(df_box) != length(playerBoxCols)) {
    stop(
      "Column mismatch: expected ", length(playerBoxCols),
      " columns but received ", ncol(df_box), "."
    )
  }

  colnames(df_box) <- playerBoxCols

  return(df_box)
}
