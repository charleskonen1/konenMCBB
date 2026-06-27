# konenMCBB 0.2.0

## New functions

### Bart Torvik

- `torvik_team_ratings()` — pulls T-Rank adjusted efficiency ratings, Barthag power
  rating, projected record, SOS, WAB, and pace for all teams in a season
- `torvik_four_factors()` — offensive and defensive four factors (eFG%, TO%, Reb%,
  FTR) for all teams
- `torvik_shooting()` — shot location splits by zone (rim, mid-range, 3PT, dunk)
  on both offense and defense; critical for shot profile and rim protection analysis
- `torvik_conf_stats()` — conference-level aggregates derived from team ratings and
  four factors; includes top team, total WAB, and mean four factor values per conference

### ESPN utilities

- `espn_teams()` — full D1 team directory from ESPN with team IDs, conference
  membership, brand colors, and logo URLs
- `espn_scoreboard()` — daily scoreboard from ESPN with scores, status, broadcast,
  and venue; works for past, present, and future dates

### ESPN season aggregation

- `espn_season_box()` — reads all saved `box_team_stats.rds` files for a season
  from the local ESPN database and returns a single combined tibble
- `espn_season_players()` — reads all saved `box_players.rds` files for a season;
  supports filtering to active players and optional date subsetting
- `espn_team_season_summary()` — per-team season averages for all box score and
  efficiency metrics, aggregated from `espn_season_box()`

### ESPN game pipeline

- `espn_game()` — fetches and parses a single ESPN game; returns a named list with
  box scores, play-by-play, win probability, betting lines, leaders, and on/off splits
- `espn_process_day()` — collects and saves all D1 games for a calendar date into
  the local file database
- `espn_load_game()` — reads a saved game from local DB without network access
- `espn_set_db_path()` — sets `options(konenMCBB.espn_db_path)` for the session

## Improvements

- All existing functions migrated from `httr` to `httr2` for modern HTTP handling
- `dayCast()`, `ncaa_pbp()`, `ncaa_player_box()` migrated to native R pipe (`|>`)
  and `.data` pronoun; `magrittr` dependency removed
- Full `roxygen2` documentation added to all exported functions
- DESCRIPTION: version bumped to 0.2.0, R >= 4.1.0 required, `httr` removed,
  `httr2` confirmed, `stats` and `tools` added to Imports
- pkgdown site config (`_pkgdown.yml`) created with Bootstrap 5, organized reference
  sections, and getting-started vignette

## Documentation

- Getting-started vignette (`vignettes/konenMCBB.Rmd`) covering all six workflows:
  Torvik ratings, ESPN pipeline setup and collection, season aggregation, cross-source
  joins, schedule/résumé analysis, and player/injury/betting data
- `README.md` with quick-start examples, full function reference table, and ESPN DB
  directory structure
- `SUPABASE_SETUP.md` (project root) — 10-step guide for setting up cloud PostgreSQL
  on Supabase including schema creation, R loading workflow, and indexing strategy

---

# konenMCBB 0.1.0

Initial release with the following functions:

- `dayCast()` — daily game digest from Bart Torvik
- `get_games()` — game-level results with quadrant classification
- `get_super_sked()` — full season schedule with advanced context
- `teamBoxScore()` — team box score from Torvik
- `player_stats()` — advanced player metrics from Torvik
- `current_resume()` — current season résumé by quadrant
- `historic_team_results()` — multi-season team result history
- `super_sked_with_timemachine()` — schedule overlaid with time machine ratings
- `timeMachine_ratings()` — Torvik ratings at a historical date snapshot
- `PlayerBoxScore()` — player game logs from Sports Reference
- `teamRoster()` — team roster from Sports Reference
- `teamList()` — team directory from Sports Reference
- `ncaa_pbp()` — play-by-play from NCAA.com
- `ncaa_player_box()` — player box scores from NCAA.com
- `injuryList()` — current injury report from Covers.com
- `espn_rankings_summary()` — AP and Coaches Poll rankings from ESPN
