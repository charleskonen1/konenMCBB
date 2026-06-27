# Getting Started with konenMCBB

## Overview

`konenMCBB` is a college basketball data package for R. It brings
together data from three primary sources:

- **Bart Torvik** (`barttorvik.com`) — the industry-standard adjusted
  efficiency ratings, four factors, shooting splits, and schedule data
  for D1 men’s basketball
- **ESPN** (`site.api.espn.com`) — live and historical game data
  including box scores, play-by-play, win probability, betting lines,
  and on/off rotations, with a local file database for offline
  season-wide analysis
- **Legacy sources** — Sports Reference game logs and rosters, NCAA.com
  PBP, and Covers.com injury reports

This vignette walks through the main workflows from installation to a
full cross-source analysis.

------------------------------------------------------------------------

## Installation

``` r
# Install from GitHub (requires devtools or pak)
devtools::install_github("charliekonen/konenMCBB")

# Or with pak
pak::pak("charliekonen/konenMCBB")
```

------------------------------------------------------------------------

## Part 1: Bart Torvik Ratings

The Torvik family of functions always take a `year` argument — the
*ending* year of the season (so 2025 means the 2024-25 season).

### Team Ratings (T-Rank)

``` r
library(konenMCBB)
library(dplyr)

# Current season ratings for all teams
ratings <- torvik_team_ratings(year = 2025)

# Top 25 by T-Rank power rating (Barthag)
ratings |>
  slice_max(barthag, n = 25) |>
  select(rank, team, conf, adj_oe, adj_de, barthag, wab)
```

`adj_oe` is points per 100 possessions on offense (adjusted for
schedule), `adj_de` is points allowed per 100 possessions, and `barthag`
is the power rating — the probability of beating an average D1 team on a
neutral floor.

### Four Factors

``` r
# Four factors: eFG%, TO rate, reb%, FT rate — both ends of the floor
ff <- torvik_four_factors(year = 2025)

# Best defenses by defensive eFG% allowed
ff |>
  arrange(d_efg) |>
  select(team, conf, d_efg, d_to_pct, d_reb_pct, d_ftr) |>
  head(20)
```

### Shooting Splits

``` r
# Shot location breakdowns: rim, mid-range, 3PT — offense and defense
shooting <- torvik_shooting(year = 2025)

# Teams that live at the rim and avoid mid-rangers
shooting |>
  filter(o_rim_rate > 0.40, o_mid_rate < 0.18) |>
  arrange(desc(o_rim_pct)) |>
  select(team, conf, o_rim_rate, o_rim_pct, o_mid_rate, o_three_rate)

# Best rim-protecting defenses
shooting |>
  arrange(d_rim_pct) |>
  select(team, conf, d_rim_pct, d_rim_rate) |>
  head(15)
```

### Conference Aggregates

``` r
# Conference-level rollup — great for bracket analysis
conf <- torvik_conf_stats(year = 2025)

conf |>
  select(conf, n_teams, mean_barthag, mean_adj_oe, mean_adj_de,
         top_team, total_wab) |>
  arrange(desc(mean_barthag))
```

### Historical Snapshots (Time Machine)

``` r
# What did teams look like at a specific point in the season?
march1 <- timeMachine_ratings(date = "03-01", year = 2025)

march1 |>
  arrange(desc(barthag)) |>
  select(rank, team, conf, barthag, adj_oe, adj_de) |>
  head(20)
```

### Combining Torvik Sources

``` r
# Comprehensive team profile: ratings + shooting
full_profile <- ratings |>
  left_join(
    shooting |> select(team, conf, starts_with("o_"), starts_with("d_")),
    by = c("team", "conf")
  ) |>
  left_join(
    ff |> select(team, conf, o_efg, d_efg, o_to_pct, d_to_pct,
                  o_reb_pct, d_reb_pct),
    by = c("team", "conf")
  )

# Elite offenses that also shoot well at the rim
full_profile |>
  filter(adj_oe > 120, o_rim_pct > 0.60) |>
  select(team, conf, adj_oe, o_rim_pct, o_rim_rate, o_efg, barthag)
```

------------------------------------------------------------------------

## Part 2: ESPN Game Data Pipeline

The ESPN pipeline stores game data locally as RDS files, organized by
season and date. This two-step workflow — collect once, read many times
— means you can run analysis without hitting the network every time.

### Step 1: Configure the Database Path

``` r
# Tell konenMCBB where to store/read ESPN game files
# Set this once per session (or add to .Rprofile)
espn_set_db_path("~/data/espn_db")
```

You can make this permanent by adding the following to your
`~/.Rprofile`:

``` r
options(konenMCBB.espn_db_path = "~/data/espn_db")
```

The database is organized as:

    espn_db/
      2024-25/
        2024-11-11/
          401703437/
            box_team_stats.rds
            box_players.rds
            game_info.rds
            plays.rds
            winprobability.rds
            pickcenter.rds
            leaders.rds
            on_off.rds

### Step 2: Discover What’s Available

``` r
# Get all D1 teams with ESPN IDs, conference info, and logos
teams <- espn_teams()

# Find a specific team
teams |> filter(grepl("Duke", display_name))

# Today's scoreboard
board <- espn_scoreboard()
board |> filter(status == "final") |>
  select(away_team_name, away_score, home_score, home_team_name)

# Games from a specific date
espn_scoreboard("2025-03-15") |>
  select(game_id, away_team_name, home_team_name, away_score, home_score, status)
```

### Step 3: Collect Games

``` r
# Fetch all D1 games on a date and save to local DB (~2-3 minutes for a big day)
espn_process_day(
  date    = "2025-01-15",
  season  = "2024-25",
  sleep_sec = 0.3    # be polite to ESPN's servers
)

# Or collect a single game directly (returns named list, no saving)
game <- espn_game(game_id = "401703437")
names(game)
# "box_team_stats" "box_players" "game_info" "leaders"
# "winprobability" "pickcenter" "plays" "on_off"
```

### Step 4: Load Individual Games

``` r
# Load a saved game without hitting the network
game <- espn_load_game(
  game_id = "401703437",
  season  = "2024-25",
  date    = "2024-11-11"
)

# Team box score
game$box_team_stats

# Player box score
game$box_players |>
  select(display_name, team_id, minutes_numeric, points,
         true_shooting_pct, plus_minus) |>
  arrange(desc(plus_minus))

# Play-by-play
game$plays |>
  filter(score_value > 0) |>
  select(period_number, clock_display_value, text, score_home, score_away)

# Win probability
game$winprobability |>
  select(seq_num, home_win_percentage, play_text) |>
  tail(20)

# Betting lines
game$pickcenter |>
  select(provider, spread, over_under, home_team_odds_moneyline)
```

### Step 5: Season-Wide Analysis

Once you’ve collected games for a season, use the aggregation functions
to load everything at once:

``` r
# All team box scores for the season (combines every saved game)
box <- espn_season_box("2024-25")

# Team efficiency leaders for the season
box |>
  group_by(team_id) |>
  summarise(
    games    = n(),
    avg_eff  = mean(eff, na.rm = TRUE),
    avg_efg  = mean(effective_field_goal_pct, na.rm = TRUE),
    avg_pace = mean(pace, na.rm = TRUE),
    .groups  = "drop"
  ) |>
  filter(games >= 10) |>
  arrange(desc(avg_eff)) |>
  head(20)

# All player box scores
players <- espn_season_players("2024-25")

# Top players by true shooting (min 15 games, min 20 min/game)
players |>
  group_by(player_id, display_name, team_id) |>
  summarise(
    games      = n(),
    avg_min    = mean(minutes_numeric, na.rm = TRUE),
    avg_pts    = mean(points, na.rm = TRUE),
    avg_ts     = mean(true_shooting_pct, na.rm = TRUE),
    avg_pm     = mean(plus_minus, na.rm = TRUE),
    .groups    = "drop"
  ) |>
  filter(games >= 15, avg_min >= 20) |>
  arrange(desc(avg_ts))

# Quick team-level season summary (wins, losses, per-game averages)
espn_team_season_summary("2024-25") |>
  select(team_id, games, wins, losses, win_pct, avg_pts, avg_eff, avg_pace) |>
  head(20)

# Load only conference tournament dates
conf_tourney <- espn_season_box(
  "2024-25",
  dates = seq(as.Date("2025-03-11"), as.Date("2025-03-16"), by = "day")
)
```

------------------------------------------------------------------------

## Part 3: Cross-Source Analysis

The real power of `konenMCBB` is combining Torvik’s adjusted metrics
with ESPN’s raw game data.

``` r
# 1. Get Torvik ratings
ratings <- torvik_team_ratings(year = 2025)
ff      <- torvik_four_factors(year = 2025)

# 2. Get ESPN season box scores (team-level)
espn_set_db_path("~/data/espn_db")
box <- espn_season_box("2024-25")

# 3. ESPN team summary (empirical season stats)
espn_summary <- espn_team_season_summary("2024-25")

# 4. Get team directory to join ESPN IDs with team names
teams <- espn_teams()

# 5. Join ESPN summary with team names
espn_named <- espn_summary |>
  left_join(teams |> select(team_id, display_name, conference_name),
            by = "team_id")

# 6. If you have a manual ESPN-to-Torvik name crosswalk (or fuzzy match):
# For now, filter to teams with a clean name match
espn_named <- espn_named |>
  mutate(team_short = sub(" (Blue Devils|Tar Heels|Wildcats|.*)", "", display_name))

# 7. Efficiency gap: Torvik adjusted OE vs. empirical eFG%
# Teams outperforming their shooting (good shot selection / tempo effects)
combined <- ratings |>
  left_join(ff |> select(team, o_efg, d_efg), by = "team")

combined |>
  filter(!is.na(o_efg)) |>
  mutate(oe_efg_ratio = adj_oe / (o_efg * 200)) |>   # rough efficiency-per-shot
  arrange(desc(oe_efg_ratio)) |>
  select(team, conf, adj_oe, o_efg, barthag, oe_efg_ratio) |>
  head(20)
```

------------------------------------------------------------------------

## Part 4: Schedule & Résumé Analysis

``` r
# Current season super schedule (all games with advanced context)
sked <- get_super_sked(year = 2025, team = "Duke")
sked |>
  select(date, opp, result, adj_oe, adj_de, barthag_diff, quad) |>
  arrange(date)

# Season resume: record by quadrant
resume <- current_resume(team = "Duke", year = 2025)

# Historical team results
historic_team_results(team = "Duke", year = 2025) |>
  select(date, opp, result, opp_barthag) |>
  filter(result == "L") |>   # loss analysis
  arrange(desc(opp_barthag))
```

------------------------------------------------------------------------

## Part 5: Player Stats & Rosters

``` r
# Advanced player stats from Torvik
duke_players <- player_stats(team = "Duke", year = 2025)
duke_players |>
  arrange(desc(bpm)) |>
  select(player, pos, min_pct, pts, reb, ast, bpm, obpm, dbpm)

# Sports Reference box scores for a player
# (requires team name in Sports-Reference convention)
PlayerBoxScore(player_name = "Cooper Flagg", team = "Duke", year = 2025)

# Team roster from Sports Reference
teamRoster(team = "Duke", year = 2025)

# NCAA.com play-by-play
ncaa_pbp(game_id = "6385585")  # NCAA game ID format differs from ESPN
```

------------------------------------------------------------------------

## Part 6: Injury & Betting Data

``` r
# Current injury list from Covers.com
injuries <- injuryList()
injuries |>
  filter(status %in% c("Out", "Doubtful")) |>
  select(player, team, status, injury, return_date)

# Betting lines are captured automatically inside espn_game():
game <- espn_load_game("401703437", "2024-25", "2024-11-11")
game$pickcenter |>
  select(provider, spread, over_under,
         home_team_odds_moneyline, away_team_odds_moneyline)
```

------------------------------------------------------------------------

## Next Steps

After collecting a season of ESPN data, consider loading it into
PostgreSQL (Supabase is recommended for cloud hosting) using the schema
in `SUPABASE_SETUP.md` at the project root. This enables:

- Cross-season queries over millions of plays
- Joining ESPN play-by-play with Torvik per-game efficiency
- Powering a public-facing stats website

See the `SUPABASE_SETUP.md` file for a 10-step guide from schema
creation to querying from R.

------------------------------------------------------------------------

## Tips

**Rate limiting** — when calling
[`espn_process_day()`](../reference/espn_process_day.md) in a loop
across many dates, use `sleep_sec = 0.3` to 0.5 to avoid hitting ESPN’s
rate limits.

**Column naming** — all konenMCBB functions return snake_case column
names. Functions from different sources use the same conventions so
joins are predictable.

**Torvik year convention** — `year = 2025` always means the 2024-25
season (the year the season ends). This matches Torvik’s own URL
convention.

**ESPN DB path** — set
`options(konenMCBB.espn_db_path = "~/data/espn_db")` in your
`~/.Rprofile` to avoid setting it every session.

**pkgdown site** — run
[`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html)
from the package root to render this vignette and all function
documentation as a local website.
