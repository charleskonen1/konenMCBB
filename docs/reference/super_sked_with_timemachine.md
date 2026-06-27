# Super schedule joined with Time Machine ratings

For a given date, retrieve the Bart Torvik super schedule and Time
Machine team ratings, and return a long data.frame where each row is a
single team-game. Each game appears twice (once from each team's
perspective), with team and opponent pre-game ratings plus the final
score.

## Usage

``` r
super_sked_with_timemachine(date)
```

## Arguments

- date:

  Date in YYYYMMDD format (must satisfy
  [`timeMachine_ratings()`](timeMachine_ratings.md)).

## Value

A data.frame with one row per team-game, including:

- `game_date`: game date as Date

- `muid`: Torvik game id

- `team`, `opponent`: team names

- `team_pts`, `opp_pts`: final score from each team's perspective

- `team_*`: selected Time Machine metrics for the team

- `opp_*`: the same metrics for the opponent

## Examples

``` r
if (FALSE) { # \dontrun{
  # 2024-11-15 slate with Time Machine ratings
  df <- super_sked_with_timemachine(20241115)
} # }
```
