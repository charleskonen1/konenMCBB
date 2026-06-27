# Get ESPN daily college basketball scoreboard

Returns all men's college basketball games on a given date from ESPN's
scoreboard API — with scores, game status, team IDs, and ESPN game IDs.
Works for past, present, and future dates.

## Usage

``` r
espn_scoreboard(date = Sys.Date(), groups = "50", timeout = 15)
```

## Arguments

- date:

  Date or character. The date to fetch (e.g. \`Sys.Date()\`,
  \`"2024-11-11"\`, or \`as.Date("2025-03-15")\`). Defaults to today.

- groups:

  Character. ESPN group filter. \`"50"\` (default) returns D1 games
  only. Use \`"100"\` for all games including lower divisions.

- timeout:

  Numeric. Request timeout in seconds. Default \`15\`.

## Value

A tibble with one row per game, containing:

- game_id:

  ESPN event ID (use with \[espn_game()\] to pull full data).

- date:

  Game date as Date.

- game_datetime:

  Tip-off time as UTC character string.

- status:

  Game status: \`"scheduled"\`, \`"in_progress"\`, or \`"final"\`.

- period:

  Current period (useful for in-progress games).

- clock:

  Game clock string for in-progress games.

- home_team_id, away_team_id:

  ESPN team IDs (join with \[espn_teams()\]).

- home_team_name, away_team_name:

  Team display names.

- home_score, away_score:

  Current or final scores.

- home_winner, away_winner:

  Logical; \`TRUE\` if that team won.

- neutral_site:

  Logical; \`TRUE\` for neutral-site games.

- conference_game:

  Logical; \`TRUE\` for conference matchups.

- venue_name:

  Arena name.

- broadcast:

  TV/streaming broadcast label (e.g. \`"ESPN"\`, \`"ESPN2"\`).

## See also

\[espn_game()\] to fetch full game data, \[espn_teams()\] for team IDs,
\[espn_process_day()\] to save all games for a date to local DB.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Today's games
  scoreboard <- espn_scoreboard()

  # A specific date
  espn_scoreboard("2024-03-15")

  # Games still in progress right now
  espn_scoreboard() |> dplyr::filter(status == "in_progress")

  # Get game IDs to pass to espn_game()
  ids <- espn_scoreboard("2024-11-11")$game_id
  game <- espn_game(ids[1])

  # Loop and fetch all games on a date
  board <- espn_scoreboard("2024-11-11")
  games <- lapply(board$game_id, espn_game)
} # }
```
