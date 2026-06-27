# Summarise team season stats from the local ESPN database

Aggregates all game-level team box scores for a season into per-team
season totals and averages. A quick way to rank teams by any box score
or efficiency metric without Postgres.

## Usage

``` r
espn_team_season_summary(season, base_path = .espn_db_base(), min_games = 5L)
```

## Arguments

- season:

  Character. Season label (e.g. \`"2024-25"\`).

- base_path:

  Character. ESPN database root path.

- min_games:

  Integer. Minimum games played to include a team. Default \`5\`.

## Value

A tibble with one row per team, containing season totals and per-game
averages for points, rebounds, assists, turnovers, efficiency, eFG

## See also

\[espn_season_box()\] for game-level data.

## Examples

``` r
if (FALSE) { # \dontrun{
  espn_set_db_path("~/data/espn_db")

  summary <- espn_team_season_summary("2024-25")
  summary |> dplyr::arrange(dplyr::desc(avg_eff))
} # }
```
