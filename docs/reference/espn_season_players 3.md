# Load all player box scores for a season from the local ESPN database

Reads every saved \`box_players.rds\` file for a season and returns one
combined tibble with one row per player per game. Includes raw box score
stats, advanced metrics, and plus/minus.

## Usage

``` r
espn_season_players(
  season,
  base_path = .espn_db_base(),
  dates = NULL,
  active_only = TRUE
)
```

## Arguments

- season:

  Character. Season label (e.g. \`"2024-25"\`).

- base_path:

  Character. ESPN database root path. Defaults to
  \`getOption("konenMCBB.espn_db_path")\`. Set with
  \[espn_set_db_path()\].

- dates:

  Optional Date vector or character vector of \`"YYYY-MM-DD"\` dates to
  include. If \`NULL\` (default), all available dates are loaded.

- active_only:

  Logical. If \`TRUE\` (default), excludes rows where \`did_not_play ==
  TRUE\`.

## Value

A tibble with one row per player per game, containing:

- season:

  Season label.

- game_date:

  Date of the game.

- game_id:

  ESPN event ID.

- team_id:

  ESPN team ID.

- player_id:

  ESPN player ID.

- display_name:

  Player name.

- position:

  Position abbreviation.

- starter:

  Logical; \`TRUE\` if the player started.

- did_not_play:

  Logical; \`TRUE\` if the player did not play.

- minutes_numeric:

  Minutes played as a decimal.

- points, field_goals_made, ...:

  Counting stats.

- effective_field_goal_pct, true_shooting_pct, ...:

  Advanced metrics.

- plus_minus:

  Plus/minus for the game.

## See also

\[espn_season_box()\] for team-level data, \[espn_process_day()\] to
collect game data.

## Examples

``` r
if (FALSE) { # \dontrun{
  espn_set_db_path("~/data/espn_db")

  players <- espn_season_players("2024-25")

  # Season averages for all players (min 10 games)
  players |>
    dplyr::group_by(player_id, display_name, team_id) |>
    dplyr::summarise(
      games         = dplyr::n(),
      avg_pts       = mean(points, na.rm = TRUE),
      avg_ts        = mean(true_shooting_pct, na.rm = TRUE),
      avg_plus_minus = mean(plus_minus, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(games >= 10) |>
    dplyr::arrange(dplyr::desc(avg_plus_minus))

  # Starters only
  starters <- espn_season_players("2024-25") |>
    dplyr::filter(starter == TRUE)
} # }
```
