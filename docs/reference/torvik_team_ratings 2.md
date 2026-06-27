# Get Bart Torvik team ratings (T-Rank)

Retrieves the current Bart Torvik T-Rank team ratings table for a given
season. This is the primary team-quality metric table from
barttorvik.com, containing adjusted offensive and defensive efficiency,
Barthag power rating, adjusted tempo, WAB, and rankings.

## Usage

``` r
torvik_team_ratings(
  year = as.integer(format(Sys.Date(), "%Y")),
  conf = "All",
  timeout = 20
)
```

## Arguments

- year:

  Integer. Season year (e.g. \`2026\` for the 2025-26 season). Must be
  \`\>= 2008\`. Defaults to the current year.

- conf:

  Character. Optional conference filter (e.g. \`"ACC"\`, \`"B10"\`). Use
  \`"All"\` (default) for all teams. See barttorvik.com for
  abbreviations.

- timeout:

  Numeric. Request timeout in seconds. Default \`20\`.

## Value

A tibble with one row per team, containing:

- rank:

  Overall T-Rank ranking.

- team:

  Team name (matches Torvik's naming convention).

- conf:

  Conference abbreviation.

- record:

  Season record (W-L).

- adj_oe:

  Adjusted offensive efficiency (points per 100 possessions vs. avg D).

- oe_rank:

  Adjusted OE rank.

- adj_de:

  Adjusted defensive efficiency (points allowed per 100 possessions vs.
  avg O).

- de_rank:

  Adjusted DE rank.

- barthag:

  Power rating — estimated win probability vs. average D1 team (0–1).

- adj_tempo:

  Adjusted pace (possessions per 40 minutes).

- proj_w, proj_l:

  Projected wins and losses.

- wab:

  Wins above bubble.

- wab_rank:

  WAB rank.

- sos:

  Strength of schedule.

- fun:

  Torvik "fun rating" — how exciting the team plays.

## Details

Ratings are opponent-adjusted using Torvik's iterative methodology. A
team with \`adj_oe = 120\` scores 120 points per 100 possessions against
an average D1 defense. \`barthag\` is the cleanest single-number
summary: a value of 0.95 means the team is expected to beat an average
team 95

Column names are cleaned to snake_case. The raw Torvik column headers
are preserved in a \`raw_name\` attribute if needed.

## See also

\[timeMachine_ratings()\] for point-in-time historical snapshots,
\[torvik_four_factors()\] for four-factor breakdown, \[player_stats()\]
for player-level advanced metrics.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Current season all teams
  ratings <- torvik_team_ratings()
  head(ratings)

  # Filter to ACC only
  acc <- torvik_team_ratings(conf = "ACC")

  # Historical season
  ratings_2024 <- torvik_team_ratings(year = 2024)
} # }
```
