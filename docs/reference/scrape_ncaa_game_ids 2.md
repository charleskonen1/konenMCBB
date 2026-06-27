# Scrape NCAA game IDs by season and date

Internal utility used to generate package data.

## Usage

``` r
scrape_ncaa_game_ids(
  season_ids,
  start_mmdd = "11-01",
  end_mmdd = "03-08",
  sleep_sec = 1
)
```

## Arguments

- season_ids:

  Vector of NCAA season division IDs

- start_mmdd:

  Start date (MM-DD)

- end_mmdd:

  End date (MM-DD)

- sleep_sec:

  Base sleep time between requests

## Value

A tibble of game IDs
