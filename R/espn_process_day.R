# Internal: get ESPN game IDs for a single date (scrape scoreboard for that day).
# Returns character vector of event IDs.
.espn_game_ids_for_date <- function(date, group = "50") {
  d <- as.Date(date)
  date_url <- format(d, "%Y%m%d")
  url <- paste0("https://www.espn.com/mens-college-basketball/scoreboard/_/date/", date_url)
  if (!is.null(group) && nzchar(as.character(group))) {
    url <- paste0(url, "/group/", as.character(group))
  }
  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) NULL
  )
  if (is.null(page)) return(character(0L))
  hrefs <- rvest::html_elements(page, "a")
  hrefs <- rvest::html_attr(hrefs, "href")
  hrefs <- stats::na.omit(hrefs)
  hrefs <- purrr::keep(hrefs, ~ stringr::str_detect(.x, "/gameId/"))
  game_ids_href <- stringr::str_extract(hrefs, "(?<=gameId/)\\d+")

  # ESPN often hydrates links in scripts; scrape ids from raw HTML as a fallback/complement.
  html_text <- as.character(page)
  game_ids_html <- stringr::str_extract_all(html_text, "(?<=gameId/)\\d+")[[1L]]

  unique(stats::na.omit(c(game_ids_href, game_ids_html)))
}

# Process one game: fetch -> save raw -> save game object -> parse and save dataframes.
.espn_process_one_game <- function(event_id, game_dir, sleep_sec = 0.5) {
  game_dir <- .espn_db_ensure_dir(game_dir)
  raw_path <- file.path(game_dir, "raw.json")
  game_path <- file.path(game_dir, "game.rds")
  if (file.exists(raw_path) && file.exists(game_path)) {
    # Already have raw and game object; still re-parse dataframes in case schema changed
    game_obj <- readRDS(game_path)
  } else {
    game_obj <- .espn_fetch_summary(event_id)
    .espn_save_raw(game_obj, game_dir)
    .espn_save_game_object(game_obj, game_dir)
    Sys.sleep(stats::runif(1, sleep_sec, sleep_sec + 0.5))
  }
  .espn_save_game_dataframes(game_obj, game_dir)
  invisible(game_dir)
}

# Create dataframes for each game on a given day.
# date: Date or character "YYYY-MM-DD". season: e.g. "2024-25".
# game_ids: optional character vector of ESPN event IDs; if NULL, scrapes scoreboard for that date.
# base_path: optional; default is .espn_db_base().
.espn_process_day <- function(date, season, game_ids = NULL, base_path = .espn_db_base(), sleep_sec = 0.5, espn_group = "50") {
  date <- as.Date(date)
  season <- as.character(season)
  base_path <- base_path
  day_dir <- .espn_db_day_dir(base_path, season, date)

  # High-level check: where data will go (cat so it always prints, e.g. in Rmd)
  base_resolved <- normalizePath(base_path, mustWork = FALSE)
  day_resolved <- normalizePath(day_dir, mustWork = FALSE)
  cat("ESPN DB base path:", base_resolved, "\n")
  cat("Day folder (data output):", day_resolved, "\n")

  if (is.null(game_ids)) {
    game_ids <- .espn_game_ids_for_date(date, group = espn_group)
    if (length(game_ids) == 0L) {
      cat("No ESPN game IDs found for ", format(date), ". Nothing written.\n", sep = "")
      return(invisible(character(0L)))
    }
    cat("Scraped ", length(game_ids), " game ID(s) for ", format(date), " from ESPN group ", espn_group, "\n", sep = "")
  } else {
    cat("Processing ", length(unique(game_ids)), " supplied game ID(s)\n", sep = "")
  }

  game_ids <- unique(as.character(game_ids))
  .espn_db_ensure_dir(day_dir)

  for (eid in game_ids) {
    game_dir <- .espn_db_game_dir(base_path, season, date, eid)
    tryCatch(
      .espn_process_one_game(eid, game_dir, sleep_sec = sleep_sec),
      error = function(e) {
        cat("Failed to process game ", eid, ": ", conditionMessage(e), "\n", sep = "")
      }
    )
  }

  # High-level check: confirm output location and contents
  if (dir.exists(day_dir)) {
    written <- list.dirs(day_dir, full.names = FALSE, recursive = FALSE)
    cat("Done. Data written to:", normalizePath(day_dir, mustWork = FALSE), "\n")
    cat("Game folders (", length(written), "): ", paste(written, collapse = ", "), "\n", sep = "")
  }

  invisible(game_ids)
}
