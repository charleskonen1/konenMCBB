# Get team box score game log (Bart Torvik)

Retrieves team-level game-by-game box score and efficiency statistics
from Bart Torvik's \`getgamestats.php\` endpoint.

## Usage

``` r
teamBoxScore(team = "All", season = 2026, timeout = 15)
```

## Arguments

- team:

  Character. Team name as used on barttorvik.com (e.g. \`"Duke"\`,
  \`"North Carolina"\`). Use \`"All"\` (default) to return all teams.

- season:

  Numeric. Season year (e.g. \`2026\` for 2025-26). Default \`2026\`.

- timeout:

  Numeric. Request timeout in seconds. Default \`15\`.

## Value

A data.frame with one row per game, containing:

- date:

  Game date.

- AwayTeam, HomeTeam:

  Team names.

- Away Fg made, Away Fg Att:

  Away field goal totals.

- Away 3pt Fg Made, Away 3pt Fg Att:

  Away three-point totals.

- Away FT made, Away FT att:

  Away free throw totals.

- Away Orb, Away Drb, Away Rbs:

  Away rebound totals.

- Away Asts, Away Steals, Away Blocks, Away Turnovers, Away Fouls:

  Away counting stats.

- Away Score, Home Score:

  Final scores.

- Home Fg made, ...:

  Same set of stats for the home team.

- Pace:

  Estimated possessions in the game.

## See also

\[get_games()\] for a cleaner efficiency-focused view per team,
\[torvik_four_factors()\] for season-aggregate four factors.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Duke's full 2025-26 game log
  duke <- teamBoxScore(team = "Duke", season = 2026)

  # All games, current season
  all_games <- teamBoxScore()
} # }
```
