# Display today's Torvik game slate

Returns today's unplayed games from the Bart Torvik super schedule,
formatted as a clean tibble sorted by game quality (TTQ). Useful for a
quick daily preview of what's on the board.

## Usage

``` r
dayCast(year = as.integer(format(Sys.Date(), "%Y")))
```

## Arguments

- year:

  Integer. Season year (e.g. \`2026\`). Defaults to the current calendar
  year, which is correct from November onward.

## Value

A tibble with one row per unplayed game today, containing:

- Matchup:

  Team vs. team label from Torvik.

- Line:

  Favored team and projected spread.

- Predicted_score:

  Torvik's predicted final score.

- WinProb:

  Win probability for the favored team.

- TTQ:

  Torvik Tier Quality — higher is a better game.

Returns the string \`"No games scheduled for today."\` if the slate is
empty.

## Examples

``` r
if (FALSE) { # \dontrun{
  dayCast()
  dayCast(year = 2025)
} # }
```
