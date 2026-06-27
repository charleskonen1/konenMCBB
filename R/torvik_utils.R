# Internal helpers -- barttorvik.com request utilities.
#
# All Torvik scraper functions route through these helpers so that
# bot-detection fixes, polite delays, and error handling only need
# to happen in one place.

# == .torvik_req ==============================================================
# Build a barttorvik.com httr2 request with browser-like headers.
# A random 1-2 second polite delay is injected before the request object is
# returned, so consecutive calls from the same session are naturally spaced out.
# Retries up to 3 times (3 s, 6 s back-off) on 429 / 503 responses.
.torvik_req <- function(url, timeout = 20,
                        referer = "https://barttorvik.com/") {
  # Polite delay -- avoids hammering the server / Cloudflare rate-limits
  Sys.sleep(stats::runif(1, 1.0, 2.0))

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
      backoff      = \(i) i * 3   # 3 s then 6 s
    )
}

# == .torvik_perform ==========================================================
# Perform a prepared barttorvik request with unified error handling.
# Distinguishes 404 (off-season / data not available) from other failures
# and surfaces an actionable message in both cases.
.torvik_perform <- function(req, year_str = NULL) {
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("404", msg, fixed = TRUE)) {
        prev_year <- if (!is.null(year_str)) {
          as.character(as.integer(year_str) - 1L)
        } else "2025"
        stop(
          "barttorvik.com returned 404 Not Found",
          if (!is.null(year_str)) paste0(" (year = ", year_str, ")") else "",
          ".\n",
          "This usually means data for this season is not on the server yet. ",
          "College basketball data typically runs November-April. ",
          "Try an earlier season year (e.g., ", prev_year, ").",
          call. = FALSE
        )
      }
      stop("Failed to reach barttorvik.com: ", msg, call. = FALSE)
    }
  )

  # httr2 < 1.0 returns the response instead of raising -- handle that too
  status <- httr2::resp_status(resp)
  if (status == 404L) {
    prev_year <- if (!is.null(year_str)) {
      as.character(as.integer(year_str) - 1L)
    } else "2025"
    stop(
      "barttorvik.com returned 404 Not Found",
      if (!is.null(year_str)) paste0(" (year = ", year_str, ")") else "",
      ".\n",
      "This usually means data for this season is not on the server yet. ",
      "Try an earlier season year (e.g., ", prev_year, ").",
      call. = FALSE
    )
  }

  httr2::resp_check_status(resp)
  resp
}

# == .torvik_check_html =======================================================
# Detect if the server returned an HTML page (Cloudflare challenge / block)
# instead of the expected JSON or CSV payload.
.torvik_check_html <- function(txt, format = "data") {
  if (grepl("^\\s*<!DOCTYPE|^\\s*<html", txt, ignore.case = TRUE)) {
    stop(
      "barttorvik.com returned an HTML page instead of ", format, ". ",
      "This usually means Cloudflare bot-detection was triggered. ",
      "Wait 30-60 seconds, then try again.",
      call. = FALSE
    )
  }
  invisible(txt)
}
