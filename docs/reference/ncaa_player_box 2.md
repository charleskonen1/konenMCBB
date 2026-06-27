# Get NCAA player box score

Retrieves player-level box score statistics for a given NCAA contest ID
from the NCAA's GraphQL API. Returns one row per player per team.

## Usage

``` r
ncaa_player_box(game_id)
```

## Arguments

- game_id:

  NCAA contest ID. The numeric ID found in NCAA.com game URLs (e.g. for
  \`https://www.ncaa.com/game/6262407/boxscore\`, use \`"6262407"\`).
  Can be character or numeric.

## Value

A tibble with one row per player, containing:

- team_id:

  NCAA team identifier.

- number:

  Jersey number.

- firstName, lastName:

  Player name components.

- position:

  Position abbreviation.

- minutesPlayed:

  Minutes played as a string (e.g. \`"32:14"\`).

- year:

  Academic year (e.g. \`"So"\`).

- elig:

  Eligibility label.

- starter:

  Logical; \`TRUE\` if the player started.

- fieldGoalsMade, fieldGoalsAttempted:

  FG totals.

- freeThrowsMade, freeThrowsAttempted:

  FT totals.

- threePointsMade, threePointsAttempted:

  3-point totals.

- offensiveRebounds, totalRebounds:

  Rebound totals.

- assists, turnovers, personalFouls, steals, blockedShots:

  Other counting stats.

- points:

  Points scored.

## Examples

``` r
if (FALSE) { # \dontrun{
  box <- ncaa_player_box("6262407")
  head(box)
} # }
```
