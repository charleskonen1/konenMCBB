# Internal: build a barttorvik.com httr2 request with browser-like headers.
# All Torvik scraper functions route through here so bot-detection fixes
# only need to happen in one place.
#
# Retries up to 3 times with increasing back-off (2 s, 4 s) on transient
# HTTP errors (429, 503) or network failures.
.torvik_req <- function(url, timeout = 20,
                        referer = "https://barttorvik.com/") {
  httr2::request(url) |>
    httr2::req_headers(
      "User-Agent" = paste0(
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
        "AppleWebKit/537.36 (KHTML, like Gecko) ",
        "Chrome/125.0.0.0 Safari/537.36"
      ),
      "Accept"          = "application/json, text/csv, text/plain, */*",
      "Accept-Language" = "en-US,en;q=0.9",
      "Referer"         = referer,
      "Cache-Control"   = "no-cache"
    ) |>
    httr2::req_timeout(timeout) |>
    httr2::req_retry(
      max_tries    = 3,
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429L, 503L),
      backoff      = \(i) i * 2   # 2 s, 4 s
    )
}
