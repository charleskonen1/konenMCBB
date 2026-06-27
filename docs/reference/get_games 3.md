# Get Bart Torvik game-level efficiency stats

Retrieves game-by-game efficiency statistics for a given team and season
from Bart Torvik's \`getgamestats.php\` endpoint. Each row is one game,
with both raw and adjusted four-factor-style efficiency numbers.

## Usage

``` r
get_games(
  team = "All",
  season = as.integer(format(Sys.Date(), "%Y")),
  quad = "All"
)
```

## Arguments

- team:

  Character. Team name as used on barttorvik.com (e.g. \`"Duke"\`,
  \`"Gonzaga"\`). Use \`"All"\` (default) to return all teams.

- season:

  Numeric or character. Season year (e.g. \`2026\` for 2025-26).
  Defaults to the current year.

- quad:

  Character or integer. Quadrant filter. One of \`1\`, \`2\`, \`3\`,
  \`4\`, or \`"All"\` (default). Quadrant is based on location-adjusted
  opponent ranking.

## Value

A cleaned data.frame with one row per game, containing:

- Date:

  Game date.

- Team, Opp:

  Team and opponent names.

- Conf, Opp_Conf:

  Conference labels.

- Venue:

  Game venue (\`H\`, \`A\`, or \`N\`).

- Result:

  Game result (\`W\` or \`L\`).

- Adj. O, Adj. D:

  Adjusted offensive and defensive efficiency for the game.

- O_EFF, O_eFG%, O_TO%, O_Reb%, O_FTR:

  Offensive four factors.

- D_EFF, D_eFG%, D_TO%, D_Reb%, D_FTR:

  Defensive four factors.

- GameScore:

  Torvik game quality score.

- Tempo:

  Possessions per 40 minutes.

- gameID:

  Torvik game identifier (useful for joins).

- Coach, OppCoach:

  Coaching staff.

- AvgScoreDiff:

  Average score differential vs. opponent's avg.

- Opp_Barthag:

  Opponent's Barthag power rating.

## See also

\[teamBoxScore()\] for raw counting stats per game, \[get_super_sked()\]
for the full schedule with predictions.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Duke's full 2025-26 game log
  duke <- get_games(team = "Duke", season = 2026)

  # All Quadrant 1 games in 2025-26
  q1 <- get_games(quad = 1)

  # Filter to wins
  duke |> dplyr::filter(Result == "W")
} # }
```
