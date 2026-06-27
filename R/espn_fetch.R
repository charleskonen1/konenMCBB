# Internal: fetch ESPN game summary JSON for one event (game) ID.
# Returns parsed list (same structure as API). Does not write to disk.

.espn_fetch_summary <- function(event_id) {
  event_id <- as.character(event_id)
  if (length(event_id) != 1L) stop("`event_id` must be a single value.")

  url <- "https://site.api.espn.com/apis/site/v2/sports/basketball/mens-college-basketball/summary"
  req <- httr2::request(url)
  req <- httr2::req_url_query(req, event = event_id)
  req <- httr2::req_headers(req, Accept = "application/json")
  req <- httr2::req_timeout(req, 15)
  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)
  txt <- httr2::resp_body_string(resp)
  jsonlite::fromJSON(txt, simplifyVector = FALSE)
}
