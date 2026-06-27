# Internal: fetch a Sports Reference page as a parsed rvest HTML document,
# using browser-like headers and polite delays to avoid 429 errors.
# Returns NULL on failure (callers should handle).
.sr_fetch_html <- function(url, timeout = 20) {
  Sys.sleep(1.5)   # Sports Reference enforces rate limits aggressively
  resp <- tryCatch(
    httr2::req_perform(
      httr2::request(url) |>
        httr2::req_headers(
          "User-Agent" = paste0(
            "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
            "AppleWebKit/537.36 (KHTML, like Gecko) ",
            "Chrome/125.0.0.0 Safari/537.36"
          ),
          "Accept"          = "text/html,application/xhtml+xml,*/*",
          "Accept-Language" = "en-US,en;q=0.9",
          "Referer"         = "https://www.sports-reference.com/cbb/"
        ) |>
        httr2::req_timeout(timeout) |>
        httr2::req_retry(
          max_tries    = 2,
          is_transient = \(resp) httr2::resp_status(resp) %in% c(429L, 503L),
          backoff      = \(i) i * 5   # 5 s, 10 s — SR is strict
        )
    ),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  tryCatch(
    rvest::read_html(httr2::resp_body_string(resp)),
    error = function(e) NULL
  )
}
