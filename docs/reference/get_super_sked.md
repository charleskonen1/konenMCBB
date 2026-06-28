# Get Bart Torvik super schedule

Retrieves the full Torvik super schedule table for a given season.
Includes predicted scores, win probabilities, game quality (TTQ), and
actual results once games have been played.

## Usage

``` r
get_super_sked(year = as.integer(format(Sys.Date(), "%Y")))
```

## Arguments

- year:

  Integer. Season year (e.g. \`2026\` for the 2025-26 season). Must be
  \`\>= 2008\`. Defaults to the current calendar year.

## Value

A data.frame with one row per scheduled game, containing columns such as
\`matchup\`, \`team1\`, \`team2\`, \`t1wp\`, \`t2wp\`, \`t1ppp\`,
\`t2ppp\`, \`prediction\`, \`ttq\`, \`gp\` (games played flag),
\`Date\`, and more. Also adds parsed columns \`favored_team\`, \`line\`,
\`predicted_score\`, \`pctChanceWin\` when \`prediction\` can be parsed.

## Details

Data is sourced from \`barttorvik.com/\<year\>\_super_sked.csv\`. The
file is only available during and shortly after the active season
(roughly November–April). Requesting a year with no data on the server
will raise an informative error suggesting you try a previous year.

## See also

\[dayCast()\] for a filtered, formatted today-only view.

## Examples

``` r
if (FALSE) { # \dontrun{
  sked <- get_super_sked(2025)
  head(sked)
} # }
```
