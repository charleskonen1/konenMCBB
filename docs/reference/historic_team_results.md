# Get historical Torvik team results

Retrieves the full team results table for a given season year from Bart
Torvik. Returns one row per team with season-aggregate metrics.

## Usage

``` r
historic_team_results(year = as.integer(format(Sys.Date(), "%Y")))
```

## Arguments

- year:

  Integer. Season year (e.g. \`2025\` for 2024-25). Must be \`\>=
  2008\`. Defaults to the current calendar year.

## Value

A data.frame of team results for the requested season.

## Details

Data sourced from \`barttorvik.com/\<year\>\_team_results.csv\`. The
file is posted for seasons from 2008 onward. A 404 error means the
season file is not available — typically because the season has not
started or the data has not been posted to Torvik's server yet.

## See also

\[current_resume()\] for the active season, \[torvik_team_ratings()\]
for the efficiency-focused T-Rank table.

## Examples

``` r
if (FALSE) { # \dontrun{
  res <- historic_team_results(2025)
  head(res)
} # }
```
