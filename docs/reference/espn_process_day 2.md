# Fetch and save all ESPN games for a given date

Scrapes ESPN for all D1 men's college basketball game IDs on a given
date, then fetches, parses, and saves each game to the local ESPN
database. Safe to re-run — already-fetched games are skipped (raw JSON
is reused).

## Usage

``` r
espn_process_day(
  date,
  season,
  game_ids = NULL,
  base_path = .espn_db_base(),
  sleep_sec = 0.5,
  espn_group = "50"
)
```

## Arguments

- date:

  Date or character \`"YYYY-MM-DD"\`. The game date to process.

- season:

  Character. Season label, e.g. \`"2024-25"\`.

- game_ids:

  Optional character vector of ESPN event IDs. If \`NULL\` (default),
  game IDs are scraped from the ESPN scoreboard for that date.

- base_path:

  Character. ESPN database root. Defaults to
  \`getOption("konenMCBB.espn_db_path")\`. Set with
  \[espn_set_db_path()\].

- sleep_sec:

  Numeric. Base sleep between requests (randomised by +0–0.5s) to avoid
  rate limiting. Default \`0.5\`.

- espn_group:

  Character. ESPN scoreboard group ID. \`"50"\` (default) is all D1;
  \`"2"\` is all divisions.

## Value

Invisible character vector of processed game IDs.

## See also

\[espn_game()\] for a single game, \[espn_rankings_summary()\] to build
team rankings after populating the local database.

## Examples

``` r
if (FALSE) { # \dontrun{
  espn_set_db_path("~/data/espn_db")

  # Fetch all D1 games on a specific date
  espn_process_day("2024-11-11", season = "2024-25")

  # Process a specific set of game IDs
  espn_process_day(
    date     = "2024-11-11",
    season   = "2024-25",
    game_ids = c("401700444", "401700453")
  )

  # Loop over a date range
  dates <- seq(as.Date("2024-11-11"), as.Date("2024-11-15"), by = "day")
  for (d in as.character(dates)) {
    espn_process_day(d, season = "2024-25")
  }
} # }
```
