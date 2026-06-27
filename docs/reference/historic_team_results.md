# Get historical Torvik team results

Retrieves the full team results table for a given season year.

## Usage

``` r
historic_team_results(year = as.integer(format(Sys.Date(), "%Y")))
```

## Arguments

- year:

  Season year (must be \>= 2008)

## Value

A data.frame of team results
