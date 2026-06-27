# Get Bart Torvik team four factors

Retrieves the four-factors efficiency table from Bart Torvik for a given
season. The four factors (eFG efficiency metrics for both offense and
defense, popularized by Dean Oliver.

## Usage

``` r
torvik_four_factors(
  year = as.integer(format(Sys.Date(), "%Y")),
  conf = "All",
  timeout = 20
)
```

## Arguments

- year:

  Integer. Season year (e.g. \`2026\` for the 2025-26 season). Must be
  \`\>= 2008\`. Defaults to the current year.

- conf:

  Character. Optional conference filter (e.g. \`"ACC"\`, \`"B10"\`). Use
  \`"All"\` (default) for all teams.

- timeout:

  Numeric. Request timeout in seconds. Default \`20\`.

## Value

A tibble with one row per team, containing:

- team:

  Team name.

- conf:

  Conference abbreviation.

- gp:

  Games played.

- adj_oe:

  Adjusted offensive efficiency.

- adj_de:

  Adjusted defensive efficiency.

- barthag:

  Power rating.

- o_efg:

  Offensive effective field goal percentage.

- o_to_pct:

  Offensive turnover percentage (lower is better).

- o_reb_pct:

  Offensive rebound percentage.

- o_ftr:

  Offensive free throw rate (FTA / FGA).

- d_efg:

  Defensive effective field goal percentage allowed.

- d_to_pct:

  Defensive turnover percentage forced (higher is better).

- d_reb_pct:

  Defensive rebound percentage.

- d_ftr:

  Defensive free throw rate allowed.

- adj_tempo:

  Adjusted pace (possessions per 40 minutes).

- wab:

  Wins above bubble.

## Details

The four factors on offense and defense tell you \*why\* a team is
efficient, not just \*that\* it is. A team can have a great adjusted OE
either because it shoots well (high eFG (high OReb table makes those
drivers visible.

## See also

\[torvik_team_ratings()\] for the full T-Rank table, \[get_games()\] for
game-level four factors.

## Examples

``` r
if (FALSE) { # \dontrun{
  # All teams, current season
  ff <- torvik_four_factors()

  # Big Ten only
  b10 <- torvik_four_factors(conf = "B10")

  # Teams sorted by defensive eFG% allowed
  ff |> dplyr::arrange(d_efg)
} # }
```
