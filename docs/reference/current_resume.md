# Get current Torvik team resume table

Returns the most current Bart Torvik team results table as a tibble.
Automatically selects the correct season year: November–December uses
\`current_year + 1\` (new season); all other months use
\`current_year\`.

## Usage

``` r
current_resume()
```

## Value

A tibble containing Torvik team metrics for the active season, or the
most recently completed season if called in the off-season.

## Details

Data sourced from \`barttorvik.com/\<year\>\_team_results.csv\`. This
file is available during and shortly after the active season. If it
returns 404 the data is not yet posted; try \`historic_team_results()\`
with an explicit previous year.

## See also

\[historic_team_results()\] for an explicit year,
\[torvik_team_ratings()\] for the live T-Rank efficiency table.

## Examples

``` r
if (FALSE) { # \dontrun{
  resume <- current_resume()
  head(resume)
} # }
```
