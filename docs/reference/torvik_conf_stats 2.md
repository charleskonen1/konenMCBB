# Get conference-level aggregate stats (Bart Torvik)

Aggregates Bart Torvik team ratings and four factors up to the
conference level, giving a bird's-eye view of how each conference stacks
up in efficiency, power rating, pace, and shooting.

## Usage

``` r
torvik_conf_stats(
  year = as.integer(format(Sys.Date(), "%Y")),
  min_teams = 4L,
  timeout = 20
)
```

## Arguments

- year:

  Integer. Season year (e.g. \`2026\` for 2025-26). Must be \`\>=
  2008\`. Defaults to the current year.

- min_teams:

  Integer. Minimum number of teams a conference must have to be included
  (filters out tiny or non-standard groupings). Default \`4\`.

- timeout:

  Numeric. Request timeout in seconds passed to underlying data
  functions. Default \`20\`.

## Value

## Details

This function pulls \[torvik_team_ratings()\] and
\[torvik_four_factors()\] internally and joins them on team name. If
Torvik's naming is slightly inconsistent between endpoints for a small
number of teams, those teams may be excluded from the four-factor
columns — the raw counts will still be correct.

## See also

\[torvik_team_ratings()\] for individual team ratings,
\[torvik_four_factors()\] for team-level four factors.

## Examples

``` r
if (FALSE) { # \dontrun{
  conf <- torvik_conf_stats()

  # Best offensive conferences
  conf |> dplyr::arrange(dplyr::desc(mean_adj_oe))

  # Conference defensive rankings
  conf |>
    dplyr::arrange(mean_adj_de) |>
    dplyr::select(conf, n_teams, mean_adj_de, mean_d_efg, mean_d_reb_pct)

  # Overall conference strength
  conf |>
    dplyr::arrange(dplyr::desc(mean_barthag)) |>
    dplyr::select(conf, n_teams, mean_barthag, top_team, total_wab)
} # }
```
