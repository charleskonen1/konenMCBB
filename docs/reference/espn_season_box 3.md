# Load all team box scores for a season from the local ESPN database

Reads every saved \`box_team_stats.rds\` file for a season, attaches
game metadata (game ID, date, season), and returns one combined tibble.
This gives you a full season of team-level box score data in one call —
ready for filtering, aggregation, and analysis.

## Usage

``` r
espn_season_box(season, base_path = .espn_db_base(), dates = NULL)
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

## Value

A tibble with one row per team per game, combining:

- season:

  Season label (e.g. \`"2024-25"\`).

- game_date:

  Date of the game.

- game_id:

  ESPN event ID.

- team_id:

  ESPN team ID.

- home_away:

  \`"home"\` or \`"away"\`.

- points, field_goals_made, field_goals_attempted, ...:

  Standard box score counting stats.

- effective_field_goal_pct, true_shooting_pct, eff, pace, ...:

  Advanced metrics computed by the ESPN pipeline.

Returns an empty tibble if no data is found for the season.

## See also

\[espn_season_players()\] for player-level data, \[espn_process_day()\]
to collect data, \[espn_load_game()\] for a single game.

## Examples

``` r
if (FALSE) { # \dontrun{
  espn_set_db_path("~/data/espn_db")

  # Full season box scores
  box <- espn_season_box("2024-25")

  # Average offensive efficiency by team
  box |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(
      games    = dplyr::n(),
      avg_eff  = mean(eff, na.rm = TRUE),
      avg_efg  = mean(effective_field_goal_pct, na.rm = TRUE),
      avg_pace = mean(pace, na.rm = TRUE)
    ) |>
    dplyr::arrange(dplyr::desc(avg_eff))

  # Load only games from a specific date range
  dates <- seq(as.Date("2025-01-01"), as.Date("2025-03-01"), by = "day")
  box_conf <- espn_season_box("2024-25", dates = dates)
} # }
```
