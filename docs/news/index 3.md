# Changelog

## konenMCBB 0.2.0

### New functions

#### Bart Torvik

- [`torvik_team_ratings()`](../reference/torvik_team_ratings.md) — pulls
  T-Rank adjusted efficiency ratings, Barthag power rating, projected
  record, SOS, WAB, and pace for all teams in a season
- [`torvik_four_factors()`](../reference/torvik_four_factors.md) —
  offensive and defensive four factors (eFG%, TO%, Reb%, FTR) for all
  teams
- [`torvik_shooting()`](../reference/torvik_shooting.md) — shot location
  splits by zone (rim, mid-range, 3PT, dunk) on both offense and
  defense; critical for shot profile and rim protection analysis
- [`torvik_conf_stats()`](../reference/torvik_conf_stats.md) —
  conference-level aggregates derived from team ratings and four
  factors; includes top team, total WAB, and mean four factor values per
  conference

#### ESPN utilities

- [`espn_teams()`](../reference/espn_teams.md) — full D1 team directory
  from ESPN with team IDs, conference membership, brand colors, and logo
  URLs
- [`espn_scoreboard()`](../reference/espn_scoreboard.md) — daily
  scoreboard from ESPN with scores, status, broadcast, and venue; works
  for past, present, and future dates

#### ESPN season aggregation

- [`espn_season_box()`](../reference/espn_season_box.md) — reads all
  saved `box_team_stats.rds` files for a season from the local ESPN
  database and returns a single combined tibble
- [`espn_season_players()`](../reference/espn_season_players.md) — reads
  all saved `box_players.rds` files for a season; supports filtering to
  active players and optional date subsetting
- [`espn_team_season_summary()`](../reference/espn_team_season_summary.md)
  — per-team season averages for all box score and efficiency metrics,
  aggregated from [`espn_season_box()`](../reference/espn_season_box.md)

#### ESPN game pipeline

- [`espn_game()`](../reference/espn_game.md) — fetches and parses a
  single ESPN game; returns a named list with box scores, play-by-play,
  win probability, betting lines, leaders, and on/off splits
- [`espn_process_day()`](../reference/espn_process_day.md) — collects
  and saves all D1 games for a calendar date into the local file
  database
- [`espn_load_game()`](../reference/espn_load_game.md) — reads a saved
  game from local DB without network access
- [`espn_set_db_path()`](../reference/espn_set_db_path.md) — sets
  `options(konenMCBB.espn_db_path)` for the session

### Improvements

- All existing functions migrated from `httr` to `httr2` for modern HTTP
  handling
- [`dayCast()`](../reference/dayCast.md),
  [`ncaa_pbp()`](../reference/ncaa_pbp.md),
  [`ncaa_player_box()`](../reference/ncaa_player_box.md) migrated to
  native R pipe (`|>`) and `.data` pronoun; `magrittr` dependency
  removed
- Full `roxygen2` documentation added to all exported functions
- DESCRIPTION: version bumped to 0.2.0, R \>= 4.1.0 required, `httr`
  removed, `httr2` confirmed, `stats` and `tools` added to Imports
- pkgdown site config (`_pkgdown.yml`) created with Bootstrap 5,
  organized reference sections, and getting-started vignette

### Documentation

- Getting-started vignette (`vignettes/konenMCBB.Rmd`) covering all six
  workflows: Torvik ratings, ESPN pipeline setup and collection, season
  aggregation, cross-source joins, schedule/résumé analysis, and
  player/injury/betting data
- `README.md` with quick-start examples, full function reference table,
  and ESPN DB directory structure
- `SUPABASE_SETUP.md` (project root) — 10-step guide for setting up
  cloud PostgreSQL on Supabase including schema creation, R loading
  workflow, and indexing strategy

------------------------------------------------------------------------

## konenMCBB 0.1.0

Initial release with the following functions:

- [`dayCast()`](../reference/dayCast.md) — daily game digest from Bart
  Torvik
- [`get_games()`](../reference/get_games.md) — game-level results with
  quadrant classification
- [`get_super_sked()`](../reference/get_super_sked.md) — full season
  schedule with advanced context
- [`teamBoxScore()`](../reference/teamBoxScore.md) — team box score from
  Torvik
- [`player_stats()`](../reference/player_stats.md) — advanced player
  metrics from Torvik
- [`current_resume()`](../reference/current_resume.md) — current season
  résumé by quadrant
- [`historic_team_results()`](../reference/historic_team_results.md) —
  multi-season team result history
- [`super_sked_with_timemachine()`](../reference/super_sked_with_timemachine.md)
  — schedule overlaid with time machine ratings
- [`timeMachine_ratings()`](../reference/timeMachine_ratings.md) —
  Torvik ratings at a historical date snapshot
- [`PlayerBoxScore()`](../reference/PlayerBoxScore.md) — player game
  logs from Sports Reference
- [`teamRoster()`](../reference/teamRoster.md) — team roster from Sports
  Reference
- [`teamList()`](../reference/teamList.md) — team directory from Sports
  Reference
- [`ncaa_pbp()`](../reference/ncaa_pbp.md) — play-by-play from NCAA.com
- [`ncaa_player_box()`](../reference/ncaa_player_box.md) — player box
  scores from NCAA.com
- [`injuryList()`](../reference/injuryList.md) — current injury report
  from Covers.com
- [`espn_rankings_summary()`](../reference/espn_rankings_summary.md) —
  AP and Coaches Poll rankings from ESPN
