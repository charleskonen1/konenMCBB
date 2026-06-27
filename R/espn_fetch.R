# Internal: fetch ESPN game summary JSON for one event (game) ID.
# Returns parsed list (same structure as API). Does not write to disk.

.espn_fetch_summary <- function(event_id) {
  event_id <- as.character(event_id)
  if (length(event_id) != 1L) stop("`event_id` must be a single value.")

  url <- "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary"
  req <- httr2::request(url) |>
    httr2::req_url_query(event = event_id) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_timeout(15) |>
    httr2::req_retry(max_tries = 3,
                     is_transient = \(r) httr2::resp_status(r) %in% c(429L, 503L),
                     backoff = \(i) i * 2)
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) stop("Failed to fetch ESPN game summary: ", conditionMessage(e))
  )
  httr2::resp_check_status(resp)
  txt <- httr2::resp_body_string(resp)
  tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) stop("Failed to parse ESPN game summary JSON: ", conditionMessage(e))
  )
}
