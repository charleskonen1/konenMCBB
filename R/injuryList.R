#' Get current NCAAB injury report
#'
#' Scrapes the current men's college basketball injury report from Covers.com.
#' Returns one row per injured player with their status and description.
#'
#' @return A data.frame with one row per injured player, containing:
#'   \describe{
#'     \item{Player}{Player name.}
#'     \item{POS}{Position abbreviation.}
#'     \item{Status}{Injury status (e.g. `"Out"`, `"Questionable"`, `"Day-to-Day"`).}
#'     \item{Description}{Plain-text injury description and timeline.}
#'     \item{Team}{Team name.}
#'   }
#'   Returns an empty data.frame if no injuries are listed.
#'
#' @details
#' Data is sourced from `covers.com/sport/basketball/ncaab/injuries`. This
#' function scrapes the page and should be used responsibly (avoid hammering
#' the endpoint in a loop). The injury list is most relevant in-season and
#' updates daily.
#'
#' @examples
#' \dontrun{
#'   injuries <- injuryList()
#'
#'   # Players listed as Out
#'   injuries |> dplyr::filter(Status == "Out")
#'
#'   # Duke injuries
#'   injuries |> dplyr::filter(Team == "Duke Blue Devils")
#' }
#'
#' @export
injuryList <- function() {

  url <- "https://www.covers.com/sport/basketball/ncaab/injuries"

  webpage <- tryCatch(
    rvest::read_html(url),
    error = function(e) stop("Failed to retrieve injury page: ", conditionMessage(e))
  )

  injury_cols <- c("Player", "POS", "Status", "Description")

  results <- list()
  ticker  <- 3

  for (i in seq_len(370)) {

    search_str  <- paste0("#content > div > div > section:nth-child(", ticker, ")")
    section_node <- rvest::html_element(webpage, search_str)

    if (is.na(section_node)) break

    injury_table <- tryCatch(
      rvest::html_table(section_node),
      error = function(e) NULL
    )
    if (is.null(injury_table)) break

    team_node <- rvest::html_element(
      webpage,
      paste0(
        "#content > div > div > section:nth-child(", ticker,
        ") div.row div.col-xs-5 div a"
      )
    )

    team_name <- if (!is.na(team_node)) {
      team_raw <- rvest::html_text2(team_node)
      sub(paste0("\n", ".*"), "", team_raw)
    } else {
      NA_character_
    }

    df_team <- as.data.frame(injury_table, stringsAsFactors = FALSE)

    if (nrow(df_team) > 1) {
      names(df_team) <- injury_cols
      df_team$Team   <- team_name
      results[[length(results) + 1]] <- df_team
    }

    ticker <- ticker + 2
  }

  if (length(results) == 0) return(data.frame())

  df <- dplyr::bind_rows(results)

  df <- dplyr::mutate(
    df,
    Description = dplyr::if_else(
      is.na(Description),
      dplyr::lead(Description),
      Description
    )
  )

  dplyr::filter(df, Description != Player)
}
