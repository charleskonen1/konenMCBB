# Get current NCAAB injury report

Scrapes the current men's college basketball injury report from
Covers.com. Returns one row per injured player with their status and
description.

## Usage

``` r
injuryList()
```

## Value

A data.frame with one row per injured player, containing:

- Player:

  Player name.

- POS:

  Position abbreviation.

- Status:

  Injury status (e.g. \`"Out"\`, \`"Questionable"\`, \`"Day-to-Day"\`).

- Description:

  Plain-text injury description and timeline.

- Team:

  Team name.

Returns an empty data.frame if no injuries are listed.

## Details

Data is sourced from \`covers.com/sport/basketball/ncaab/injuries\`.
This function scrapes the page and should be used responsibly (avoid
hammering the endpoint in a loop). The injury list is most relevant
in-season and updates daily.

## Examples

``` r
if (FALSE) { # \dontrun{
  injuries <- injuryList()

  # Players listed as Out
  injuries |> dplyr::filter(Status == "Out")

  # Duke injuries
  injuries |> dplyr::filter(Team == "Duke Blue Devils")
} # }
```
