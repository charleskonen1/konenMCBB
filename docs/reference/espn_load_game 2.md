# Load a parsed ESPN game from the local database

Reads previously saved game data from the local ESPN database without
making any network requests. Useful for fast offline analysis after data
has been collected with \[espn_process_day()\].

## Usage

``` r
espn_load_game(game_id, season, date, base_path = .espn_db_base())
```

## Arguments

- game_id:

  Character or numeric. ESPN event (game) ID.

- season:

  Character. Season label (e.g. \`"2024-25"\`).

- date:

  Date or character \`"YYYY-MM-DD"\`. Game date.

- base_path:

  Character. ESPN database root path.

## Value

A named list identical in structure to the return value of
\[espn_game()\].

## Examples

``` r
if (FALSE) { # \dontrun{
  espn_set_db_path("~/data/espn_db")
  game <- espn_load_game("401700444", season = "2024-25", date = "2024-11-11")
  game$box_team_stats
} # }
```
