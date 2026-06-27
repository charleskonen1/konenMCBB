#' Scrape ESPN game IDs by season and date
#'
#' Internal utility used to generate package data.
#'
#' @param start_mmdd Start date (MM-DD).
#' @param end_mmdd End date (MM-DD).
#' @param sleep_sec Base sleep time between requests.
#' @param group ESPN scoreboard group id (default "50").
#'
#' @return A tibble of ESPN game IDs.
#' @keywords internal
espnGameIds <- function(
    start_mmdd = "11-01",
    end_mmdd   = "03-08",
    sleep_sec  = 1.5,
    group      = "50"
) {

  season_map <- tibble::tibble(
    season_id = c(2022, 2023, 2024, 2025, 2026),
    season = c(
      "2021-22",
      "2022-23",
      "2023-24",
      "2024-25",
      "2025-26"
    )
  )

  results <- list()

  for (i in seq_len(nrow(season_map))) {

    season_id    <- season_map$season_id[i]
    season_label <- season_map$season[i]

    start_year <- as.integer(substr(season_label, 1, 4))
    end_year   <- start_year + 1

    dates <- seq.Date(
      from = as.Date(paste0(start_year, "-", start_mmdd)),
      to   = as.Date(paste0(end_year, "-", end_mmdd)),
      by   = "day"
    )

    for (d in dates) {

      d_date   <- as.Date(d)
      date_url <- format(d_date, "%Y%m%d")

      url <- paste0("https://www.espn.com/mens-college-basketball/scoreboard/_/date/", date_url)
      if (!is.null(group) && nzchar(as.character(group))) {
        url <- paste0(url, "/group/", as.character(group))
      }

      message("Scraping ESPN: ", season_label, " | ", d_date)

      page <- tryCatch(
        rvest::read_html(url),
        error = function(e) NULL
      )

      if (is.null(page)) {
        Sys.sleep(stats::runif(1, sleep_sec, sleep_sec + 1))
        next
      }

      hrefs <- rvest::html_elements(page, "a")
      hrefs <- rvest::html_attr(hrefs, "href")
      hrefs <- stats::na.omit(hrefs)

      hrefs <- purrr::keep(hrefs, ~ stringr::str_detect(.x, "/gameId/"))
      game_ids_href <- stringr::str_extract(hrefs, "(?<=gameId/)\\d+")

      # ESPN may render game links in script payloads; scan raw HTML too.
      html_text <- as.character(page)
      game_ids_html <- stringr::str_extract_all(html_text, "(?<=gameId/)\\d+")[[1L]]

      game_ids <- unique(stats::na.omit(c(game_ids_href, game_ids_html)))

      if (length(game_ids) > 0) {

        results[[length(results) + 1]] <- tibble::tibble(
          date = rep(d_date, length(game_ids)),
          game_id = as.character(game_ids),
          season_id = season_id,
          season = season_label
        )
      }

      Sys.sleep(stats::runif(1, sleep_sec, sleep_sec + 1))
    }
  }

  dplyr::bind_rows(results)
}
