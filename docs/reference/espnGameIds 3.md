# Scrape ESPN game IDs by season and date

Internal utility used to generate package data.

## Usage

``` r
espnGameIds(
  start_mmdd = "11-01",
  end_mmdd = "03-08",
  sleep_sec = 1.5,
  group = "50"
)
```

## Arguments

- start_mmdd:

  Start date (MM-DD).

- end_mmdd:

  End date (MM-DD).

- sleep_sec:

  Base sleep time between requests.

- group:

  ESPN scoreboard group id (default "50").

## Value

A tibble of ESPN game IDs.
