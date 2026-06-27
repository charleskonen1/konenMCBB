# Fetch and parse a single ESPN game

Retrieves a game from ESPN's public API and returns all parsed data
frames in a named list. Optionally saves to the local ESPN database for
reuse.

## Usage

``` r
espn_game(
  game_id,
  save = FALSE,
  season = NULL,
  date = NULL,
  base_path = .espn_db_base()
)
```

## Arguments

- game_id:

  Character or numeric. ESPN event (game) ID. These can be found in ESPN
  game URLs:
  \`https://www.espn.com/mens-college-basketball/game/\_/gameId/401700444\`.

- save:

  Logical. If \`TRUE\`, save the raw JSON and parsed data frames to the
  local ESPN database. Requires \`season\` and optionally \`date\`.
  Default \`FALSE\`.

- season:

  Character. Season label (e.g. \`"2024-25"\`). Required if \`save =
  TRUE\`.

- date:

  Date or character \`"YYYY-MM-DD"\`. Game date. Required if \`save =
  TRUE\`.

- base_path:

  Character. ESPN database root path. Defaults to
  \`getOption("konenMCBB.espn_db_path")\`. Set with
  \[espn_set_db_path()\].

## Value

A named list with the following tibble elements:

- \`box_team_stats\`:

  Team-level box score with advanced metrics (2 rows).

- \`box_players\`:

  Player-level box score with advanced metrics.

- \`game_info\`:

  Venue, attendance, and officials.

- \`leaders\`:

  Stat leaders per category per team.

- \`winprobability\`:

  Win probability by play.

- \`pickcenter\`:

  Betting lines and spreads per provider.

- \`plays\`:

  Full play-by-play with shot coordinates.

- \`on_off\`:

  Player rotation stints with plus/minus.

## See also

\[espn_process_day()\] to fetch and save all games for a date,
\[espn_rankings_summary()\] to build team rankings from the local DB.

## Examples

``` r
if (FALSE) { # \dontrun{
  game <- espn_game("401700444")

  # Team box score
  game$box_team_stats

  # Player box score
  game$box_players

  # Win probability chart data
  game$winprobability

  # Save to local DB for later use
  espn_set_db_path("~/data/espn_db")
  game <- espn_game("401700444", save = TRUE, season = "2024-25", date = "2024-11-11")
} # }
```
