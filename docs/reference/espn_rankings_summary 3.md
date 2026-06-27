# Build Incremental ESPN Season Rankings Summary

Creates a daily team rankings table from the local ESPN game database.
If a rankings state file exists for the previous day, the function
reuses that state and only processes newly available day folders.

## Usage

``` r
espn_rankings_summary(
  season,
  date = Sys.Date(),
  base_path = .espn_db_base(),
  use_incremental = TRUE,
  save_state = TRUE
)
```

## Arguments

- season:

  Season label (for example, \`"2024-25"\`).

- date:

  Target date (\`Date\` or \`"YYYY-MM-DD"\`). Rankings include games up
  to and including this date.

- base_path:

  Base ESPN DB path. Defaults to \`options(konenMCBB.espn_db_path)\`
  fallback used by internal helpers.

- use_incremental:

  If \`TRUE\`, attempt to load the previous day's saved rankings state
  and append only newer days.

- save_state:

  If \`TRUE\`, write a state file at
  \`\<base_path\>/\<season\>/rankings/\<date\>.rds\`.

## Value

A tibble with team-level cumulative stats and ranks.
