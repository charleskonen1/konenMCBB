#' Scrape NCAA game IDs by season and date
#'
#' Internal utility used to generate package data.
#'
#' @param season_ids Vector of NCAA season division IDs
#' @param start_mmdd Start date (MM-DD)
#' @param end_mmdd End date (MM-DD)
#' @param sleep_sec Base sleep time between requests
#'
#' @return A tibble of game IDs
#' @keywords internal
scrape_ncaa_game_ids <- function(
    season_ids,
    start_mmdd = "11-01",
    end_mmdd   = "03-08",
    sleep_sec  = 1
) {

  season_map <- tibble::tibble(
    season_id = c(18703, 18403, 18221),
    season = c("2025-26", "2024-25", "2023-24")
  )

  results <- list()

  for (sid in season_ids) {

    idx <- which(season_map$season_id == sid)
    if (length(idx) == 0) next

    season_label <- season_map$season[idx]

    start_year <- as.integer(substr(season_label, 1, 4))
    end_year   <- start_year + 1

    dates <- seq.Date(
      from = as.Date(paste0(start_year, "-", start_mmdd)),
      to   = as.Date(paste0(end_year, "-", end_mmdd)),
      by   = "day"
    )

    for (d in dates) {

      d_date   <- as.Date(d)
      date_str <- strftime(d_date, "%m/%d/%Y")
      date_url <- utils::URLencode(date_str)

      url <- paste0(
        "https://stats.ncaa.org/season_divisions/",
        sid,
        "/livestream_scoreboards?utf8=%E2%9C%93&season_division_id=&game_date=",
        date_url,
        "&conference_id="
      )

      message("Scraping: ", season_label, " | ", date_str)

      page <- tryCatch(
        rvest::read_html(url),
        error = function(e) NULL
      )

      if (is.null(page)) {
        Sys.sleep(stats::runif(1, sleep_sec, sleep_sec + 1.5))
        next
      }

      nodes <- rvest::html_elements(page, "tr[id^='contest_']")
      ids   <- rvest::html_attr(nodes, "id")

      game_ids <- stringr::str_remove(ids, "^contest_")

      if (length(game_ids) > 0) {

        results[[length(results) + 1]] <- tibble::tibble(
          date = rep(d_date, length(game_ids)),
          game_id = as.character(game_ids),
          season_id = sid,
          season = season_label
        )
      }

      Sys.sleep(stats::runif(1, sleep_sec, sleep_sec + 1.5))
    }
  }

  dplyr::bind_rows(results)
}
