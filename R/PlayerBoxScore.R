#' Get player box score game log
#'
#' Retrieves a player's season game log from Sports Reference.
#'
#' @param name Player name (e.g., "Zach Edey")
#' @param season Season year (numeric, e.g., 2024)
#' @param team Team name to match
#'
#' @return A data.frame of game logs
#' @export
PlayerBoxScore <- function(name, season, team) {

  if (!is.character(name) || length(name) != 1) {
    stop("`name` must be a single character string.")
  }

  if (!is.numeric(season) || length(season) != 1) {
    stop("`season` must be a single numeric year.")
  }

  if (!is.character(team) || length(team) != 1) {
    stop("`team` must be a single character string.")
  }

  name_str <- gsub(" ", "-", name)
  year_str <- as.character(season)

  playerBoxCols <- c(
    "Row","Gcar","GameNum","Date","Team","Venue",
    "Opponent","TypeGame","Score","GS","MP",
    "FG","FGA","FG%","3P","3PA","3P%",
    "2P","2PA","2P%","eFG%",
    "FT","FTA","FT%",
    "ORB","DRB","TRB",
    "AST","STL","BLK",
    "TOV","PF","PTS","GmSc"
  )

  for (ticker in seq_len(5)) {

    url <- paste0(
      "https://www.sports-reference.com/cbb/players/",
      name_str, "-", ticker,
      "/gamelog/", year_str, "/"
    )

    webpage <- .sr_fetch_html(url)   # includes 1.5 s polite delay

    if (is.null(webpage)) {
      Sys.sleep(2)
      next
    }

    node <- rvest::html_element(webpage, "tbody")

    if (is.na(node)) {
      Sys.sleep(1)
      next
    }

    table <- tryCatch(
      rvest::html_table(node, fill = TRUE),
      error = function(e) NULL
    )

    if (is.null(table)) {
      Sys.sleep(1)
      next
    }

    df_box <- as.data.frame(
      table,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    if (ncol(df_box) != length(playerBoxCols)) {
      next
    }

    colnames(df_box) <- playerBoxCols

    if (!is.null(df_box$Team) &&
        length(df_box$Team) > 0 &&
        identical(df_box$Team[1], team)) {

      return(df_box)
    }

    Sys.sleep(1)
  }

  stop("No matching player/team found for given inputs.")
}
