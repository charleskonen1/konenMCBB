# Get Bart Torvik advanced player stats

Retrieves the full advanced player statistics table from Bart Torvik for
a given season. Includes efficiency, usage, shot location, BPM, and
recruiting data for all D1 players.

## Usage

``` r
player_stats(year = as.integer(format(Sys.Date(), "%Y")))
```

## Arguments

- year:

  Numeric. Season year (e.g. \`2026\` for the 2025-26 season). Must be
  \`\>= 2008\`. Defaults to the current calendar year.

## Value

A tibble with one row per player, containing:

- player_name:

  Player name.

- team:

  Team name (Torvik convention).

- conf:

  Conference abbreviation.

- GP:

  Games played.

- Min_pct:

  Percentage of team minutes played.

- ORtg:

  Offensive rating (points produced per 100 possessions used).

- usg:

  Usage rate — percentage of team possessions used.

- eFG:

  Effective field goal percentage.

- TS_pct:

  True shooting percentage.

- ORB_pct, DRB_pct:

  Offensive and defensive rebound percentages.

- AST_pct:

  Assist percentage.

- TO_pct:

  Turnover percentage.

- FTM, FTA, FT_pct:

  Free throw totals and percentage.

- twoPM, twoPA, twoP_pct:

  Two-point totals.

- TPM, TPA, TP_pct:

  Three-point totals.

- blk_pct, stl_pct, ftr_pct:

  Block, steal, and free throw rate percentages.

- yr:

  Academic year (Fr, So, Jr, Sr, Gr).

- ht:

  Height.

- porpag:

  Points over replacement per adjusted game.

- adjoe:

  Adjusted offensive efficiency for the player's team.

- bpm, obpm, dbpm, gbpm:

  Box plus/minus variants.

- rimmade, rimmade_rimmiss:

  Rim shooting totals.

- midmade, midmade_midmiss:

  Mid-range shooting totals.

- dunksmade, dunksmiss_dunksmade:

  Dunk totals.

- oreb, dreb, treb, ast, stl, blk, pts:

  Season counting stats.

- role:

  Torvik role classification.

## Details

BPM variants: \`bpm\` is total box plus/minus; \`obpm\` is offensive
BPM; \`dbpm\` is defensive BPM; \`gbpm\` is the "grades" BPM variant
used for player-to-player comparisons.

## See also

\[torvik_team_ratings()\] for team-level aggregates,
\[PlayerBoxScore()\] for game-by-game logs from Sports Reference.

## Examples

``` r
if (FALSE) { # \dontrun{
  # Current season
  players <- player_stats()

  # Top offensive players by ORtg (min 20% usage)
  players |>
    dplyr::filter(usg >= 20) |>
    dplyr::arrange(dplyr::desc(ORtg)) |>
    dplyr::select(player_name, team, ORtg, usg, eFG, TS_pct)

  # 2024 season
  p2024 <- player_stats(year = 2024)
} # }
```
