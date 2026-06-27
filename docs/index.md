# konenMCBB ![](reference/figures/logo.png)

> College basketball data digestion for R — Torvik, ESPN, Sports
> Reference, NCAA, and more in one package.

------------------------------------------------------------------------

## Installation

``` r
# Install from GitHub (requires remotes)
remotes::install_github("charleskonen1/konenMCBB")
```

------------------------------------------------------------------------

## Overview

`konenMCBB` provides a unified interface for pulling, parsing, and
storing men’s college basketball data from five sources:

| Source | What you get |
|----|----|
| **Bart Torvik** | T-Rank power ratings, four factors, adjusted efficiency, player BPM, schedule + predictions |
| **ESPN** | Box scores, play-by-play, win probability, betting lines, on/off rotations — saved locally |
| **Sports Reference** | Player game logs, team rosters, historical team directory |
| **NCAA.com** | Native play-by-play and player box scores |
| **Covers.com** | Current injury reports |

------------------------------------------------------------------------

## Quick Start

``` r
library(konenMCBB)

# --- Torvik T-Rank ratings ---
ratings <- torvik_team_ratings(year = 2026)
ratings |> dplyr::arrange(rank) |> dplyr::select(rank, team, adj_oe, adj_de, barthag, wab)

# --- Four factors ---
ff <- torvik_four_factors(year = 2026)
ff |> dplyr::arrange(d_efg) |> dplyr::select(team, conf, o_efg, d_efg, o_to_pct, d_to_pct)

# --- Today's slate ---
dayCast()

# --- Player advanced stats ---
players <- player_stats(year = 2026)
players |>
  dplyr::filter(usg >= 20, GP >= 10) |>
  dplyr::arrange(dplyr::desc(bpm)) |>
  dplyr::select(player_name, team, bpm, obpm, dbpm, ORtg, usg)

# --- Historical ratings snapshot (Time Machine) ---
snapshot <- timeMachine_ratings(date = 20250301)

# --- Super schedule with pre-game ratings ---
matchups <- super_sked_with_timemachine(date = 20250301)
```

------------------------------------------------------------------------

## Torvik Functions

### `torvik_team_ratings(year, conf)`

The main T-Rank table. Every team’s adjusted offensive and defensive
efficiency, Barthag power rating, adjusted tempo, WAB, and rankings.

``` r
torvik_team_ratings(year = 2026)
torvik_team_ratings(year = 2026, conf = "ACC")
```

**Key columns:** `rank`, `team`, `adj_oe`, `adj_de`, `barthag`,
`adj_tempo`, `wab`, `sos`

------------------------------------------------------------------------

### `torvik_four_factors(year, conf)`

The four factors that explain *why* a team is efficient: shooting
(eFG%), ball security (TO%), rebounding (Reb%), and getting to the line
(FT Rate) — for both offense and defense.

``` r
torvik_four_factors(year = 2026)
torvik_four_factors(year = 2026, conf = "B10")
```

**Key columns:** `o_efg`, `d_efg`, `o_to_pct`, `d_to_pct`, `o_reb_pct`,
`d_reb_pct`, `o_ftr`, `d_ftr`

------------------------------------------------------------------------

### `timeMachine_ratings(date)`

Point-in-time ratings for any date back to 2014. What did teams look
like on the day they played? Essential for retroactive analysis.

``` r
timeMachine_ratings(date = 20250115)
```

------------------------------------------------------------------------

### `super_sked_with_timemachine(date)`

Joins that day’s schedule with the pre-game Torvik Time Machine ratings
for each team. One row per team per game — perfect for matchup modeling.

``` r
matchups <- super_sked_with_timemachine(date = 20250301)
# columns: team, opponent, team_barthag, opp_barthag, team_adjoe, opp_adjde, ...
```

------------------------------------------------------------------------

### `get_super_sked(year)`

The full Torvik super schedule: every game with predictions, lines,
pace, quality tier (TTQ), and results.

------------------------------------------------------------------------

### `get_games(team, season, quad)`

Game-by-game four-factor efficiency per team. Filter by quadrant (1–4)
to isolate quality wins/losses.

``` r
get_games(team = "Duke", season = 2026)
get_games(quad = 1, season = 2026)  # all Q1 games
```

------------------------------------------------------------------------

### `player_stats(year)`

Full Bart Torvik player table: ORtg, usage, BPM, shot location
(rim/mid/3pt%), and more for every D1 player.

``` r
player_stats(year = 2026) |>
  dplyr::filter(usg >= 25, GP >= 15) |>
  dplyr::arrange(dplyr::desc(obpm))
```

------------------------------------------------------------------------

### `dayCast(year)`

Today’s unplayed games sorted by game quality (TTQ), with lines and win
probabilities.

``` r
dayCast()  # prints today's slate
```

------------------------------------------------------------------------

## ESPN Functions

The ESPN pipeline fetches live game data and stores it in a structured
local database. Set the database path once per session:

``` r
espn_set_db_path("~/data/espn_db")
# or in .Rprofile:
# options(konenMCBB.espn_db_path = "~/data/espn_db")
```

### Database structure

    espn_db/
      2024-25/
        2024-11-11/
          401700444/
            raw.json            # raw ESPN API response
            game.rds            # parsed R list
            box_team_stats.rds  # team box score + advanced metrics
            box_players.rds     # player box score + advanced metrics
            plays.rds           # full play-by-play with shot coordinates
            winprobability.rds  # win probability by play
            pickcenter.rds      # betting lines per provider
            on_off.rds          # player rotation stints
            leaders.rds         # stat leaders
            game_info.rds       # venue, attendance, officials

### Collecting data

``` r
# Fetch all D1 games on a specific date
espn_process_day("2024-11-11", season = "2024-25")

# Loop a date range
dates <- seq(as.Date("2024-11-11"), as.Date("2024-12-31"), by = "day")
for (d in as.character(dates)) {
  espn_process_day(d, season = "2024-25")
}
```

### Using data

``` r
# Fetch a single game live (no save)
game <- espn_game("401700444")
game$box_team_stats
game$plays
game$winprobability

# Load from local DB (offline, fast)
game <- espn_load_game("401700444", season = "2024-25", date = "2024-11-11")

# Build team rankings from everything in the DB
rankings <- espn_rankings_summary(season = "2024-25", date = Sys.Date())
```

### What’s in a game object

Each game returns a named list:

| Element | Rows | Description |
|----|----|----|
| `box_team_stats` | 2 | Team box score + eFG%, TS%, possessions, pace, per-100 rates |
| `box_players` | ~20–30 | Player box score + advanced metrics + plus/minus |
| `plays` | ~120–200 | Every play with clock, score, shot coordinates, play type |
| `winprobability` | ~120–200 | Home win % at each play event |
| `pickcenter` | 1–5 | Spread, moneyline, over/under per betting provider |
| `on_off` | varies | Player stint times with plus/minus |
| `leaders` | varies | Stat leaders per category |
| `game_info` | 1 | Venue, attendance, officials |

### Loading into PostgreSQL

Once you have local data, load it into PostgreSQL (e.g. Supabase) using
the included script:

``` bash
# Set environment variables first
export PGHOST=db.xxxx.supabase.co
export PGPORT=5432
export PGDBNAME=postgres
export PGUSER=postgres
export PGPASSWORD=your_password
export ESPN_DB_PATH=/path/to/espn_db

Rscript load_espn_db_to_postgres.R
```

See `SUPABASE_SETUP.md` for step-by-step database setup instructions.

------------------------------------------------------------------------

## Sports Reference Functions

``` r
# Player game log (e.g. for a player's season)
PlayerBoxScore(name = "Cooper Flagg", season = 2025, team = "Duke")

# Team roster
teamRoster(team = "Duke", season = 2025)

# All historical teams
teamList()
```

> **Note:** Sports Reference rate-limits scrapers. Add delays if calling
> in a loop.

------------------------------------------------------------------------

## NCAA Functions

``` r
# Play-by-play (uses NCAA.com contest ID)
pbp <- ncaa_pbp("6262407")

# Player box score
box <- ncaa_player_box("6262407")
```

NCAA contest IDs differ from ESPN game IDs. They’re found in NCAA.com
game URLs.

------------------------------------------------------------------------

## Injury Data

``` r
injuries <- injuryList()
injuries |> dplyr::filter(Status == "Out")
```

------------------------------------------------------------------------

## Function Reference

| Function | Source | Returns |
|----|----|----|
| [`torvik_team_ratings()`](reference/torvik_team_ratings.md) | Torvik | T-Rank table |
| [`torvik_four_factors()`](reference/torvik_four_factors.md) | Torvik | Four factors table |
| [`timeMachine_ratings()`](reference/timeMachine_ratings.md) | Torvik | Point-in-time ratings |
| [`super_sked_with_timemachine()`](reference/super_sked_with_timemachine.md) | Torvik | Schedule + pre-game ratings |
| [`get_super_sked()`](reference/get_super_sked.md) | Torvik | Full schedule with predictions |
| [`get_games()`](reference/get_games.md) | Torvik | Game-by-game efficiency |
| [`teamBoxScore()`](reference/teamBoxScore.md) | Torvik | Team box score log |
| [`player_stats()`](reference/player_stats.md) | Torvik | Player advanced stats |
| [`current_resume()`](reference/current_resume.md) | Torvik | Current season team metrics |
| [`historic_team_results()`](reference/historic_team_results.md) | Torvik | Season team results CSV |
| [`dayCast()`](reference/dayCast.md) | Torvik | Today’s slate |
| [`espn_set_db_path()`](reference/espn_set_db_path.md) | ESPN | Configure DB path |
| [`espn_game()`](reference/espn_game.md) | ESPN | Fetch + parse single game |
| [`espn_process_day()`](reference/espn_process_day.md) | ESPN | Fetch + save all games for a date |
| [`espn_load_game()`](reference/espn_load_game.md) | ESPN | Load from local DB |
| [`espn_rankings_summary()`](reference/espn_rankings_summary.md) | ESPN | Build team rankings |
| [`PlayerBoxScore()`](reference/PlayerBoxScore.md) | Sports Ref | Player game log |
| [`teamRoster()`](reference/teamRoster.md) | Sports Ref | Team roster |
| [`teamList()`](reference/teamList.md) | Sports Ref | Team directory |
| [`ncaa_pbp()`](reference/ncaa_pbp.md) | NCAA | Play-by-play |
| [`ncaa_player_box()`](reference/ncaa_player_box.md) | NCAA | Player box score |
| [`injuryList()`](reference/injuryList.md) | Covers | Injury report |

------------------------------------------------------------------------

## Data Sources

- **Bart Torvik**: [barttorvik.com](https://barttorvik.com) — all Torvik
  functions
- **ESPN**: [site.api.espn.com](https://site.api.espn.com) — ESPN
  functions
- **Sports Reference**:
  [sports-reference.com/cbb](https://www.sports-reference.com/cbb/) — SR
  functions
- **NCAA**: [sdataprod.ncaa.com](https://sdataprod.ncaa.com) — NCAA
  functions
- **Covers**: [covers.com](https://www.covers.com) — injury data

------------------------------------------------------------------------

## License

MIT © Charlie Konen
