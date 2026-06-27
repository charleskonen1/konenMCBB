# Get NCAA play-by-play data

Retrieves play-by-play data for a given NCAA contest ID from the NCAA's
GraphQL API. Returns one row per play event, including clock, score, and
event description.

## Usage

``` r
ncaa_pbp(game_id)
```

## Arguments

- game_id:

  NCAA contest ID. This is the numeric ID found in NCAA.com game URLs
  (e.g. for \`https://www.ncaa.com/game/6262407/play-by-play\`, the ID
  is \`"6262407"\`). Can be character or numeric.

## Value

A tibble with one row per play event, containing:

- period:

  Period number (1 = first half, 2 = second half, 3+ = OT).

- clock:

  Time remaining in the period as a string (e.g. \`"14:23"\`).

- teamId:

  NCAA team identifier for the acting team.

- isHome:

  Logical; \`TRUE\` if the acting team is the home team.

- homeScore:

  Home team score after this play.

- visitorScore:

  Away team score after this play.

- eventDescription:

  Plain-text description of the play.

## Examples

``` r
if (FALSE) { # \dontrun{
  pbp <- ncaa_pbp("6262407")
  head(pbp)
} # }
```
