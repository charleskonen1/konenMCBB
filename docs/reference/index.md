# Package index

## Bart Torvik — Team Ratings

T-Rank power ratings, four factors, shooting splits, conference
aggregates, and historical snapshots. Start here for team quality
analysis.

- [`torvik_team_ratings()`](torvik_team_ratings.md) : Get Bart Torvik
  team ratings (T-Rank)
- [`torvik_four_factors()`](torvik_four_factors.md) : Get Bart Torvik
  team four factors
- [`torvik_shooting()`](torvik_shooting.md) : Get Bart Torvik team
  shooting splits
- [`torvik_conf_stats()`](torvik_conf_stats.md) : Get conference-level
  aggregate stats (Bart Torvik)
- [`timeMachine_ratings()`](timeMachine_ratings.md) : Get historical
  Torvik Time Machine ratings
- [`super_sked_with_timemachine()`](super_sked_with_timemachine.md) :
  Super schedule joined with Time Machine ratings

## Bart Torvik — Schedules & Game Stats

Game-by-game efficiency, super schedule, and season results.

- [`get_super_sked()`](get_super_sked.md) : Get Bart Torvik super
  schedule
- [`get_games()`](get_games.md) : Get Bart Torvik game-level efficiency
  stats
- [`teamBoxScore()`](teamBoxScore.md) : Get team box score game log
  (Bart Torvik)
- [`dayCast()`](dayCast.md) : Display today's Torvik game slate
- [`current_resume()`](current_resume.md) : Get current Torvik team
  resume table
- [`historic_team_results()`](historic_team_results.md) : Get historical
  Torvik team results

## Bart Torvik — Player Stats

Advanced player efficiency, BPM, and shot location data.

- [`player_stats()`](player_stats.md) : Get Bart Torvik advanced player
  stats

## ESPN — Game Data Pipeline

Fetch, parse, and store ESPN game data (box scores, play-by-play, win
probability, betting lines, on/off rotations) in a local database.

- [`espn_set_db_path()`](espn_set_db_path.md) : Set the ESPN local
  database path
- [`espn_teams()`](espn_teams.md) : Get ESPN men's college basketball
  team directory
- [`espn_scoreboard()`](espn_scoreboard.md) : Get ESPN daily college
  basketball scoreboard
- [`espn_game()`](espn_game.md) : Fetch and parse a single ESPN game
- [`espn_process_day()`](espn_process_day.md) : Fetch and save all ESPN
  games for a given date
- [`espn_load_game()`](espn_load_game.md) : Load a parsed ESPN game from
  the local database
- [`espn_rankings_summary()`](espn_rankings_summary.md) : Build
  Incremental ESPN Season Rankings Summary

## ESPN — Season Aggregation

Read and combine a full season of saved ESPN data from the local
database into flat tibbles for offline analysis. No network required.

- [`espn_season_box()`](espn_season_box.md) : Load all team box scores
  for a season from the local ESPN database
- [`espn_season_players()`](espn_season_players.md) : Load all player
  box scores for a season from the local ESPN database
- [`espn_team_season_summary()`](espn_team_season_summary.md) :
  Summarise team season stats from the local ESPN database

## Sports Reference

Player game logs, team rosters, and historical team directory.

- [`PlayerBoxScore()`](PlayerBoxScore.md) : Get player box score game
  log
- [`teamRoster()`](teamRoster.md) : Get team roster from Sports
  Reference
- [`teamList()`](teamList.md) : Get NCAA team list from Sports Reference

## NCAA.com

Play-by-play and box scores direct from NCAA.com game data API.

- [`ncaa_pbp()`](ncaa_pbp.md) : Get NCAA play-by-play data
- [`ncaa_player_box()`](ncaa_player_box.md) : Get NCAA player box score

## Injury Data

Current injury report from Covers.com.

- [`injuryList()`](injuryList.md) : Get current NCAAB injury report

## Package

- [`konenMCBB`](konenMCBB-package.md)
  [`konenMCBB-package`](konenMCBB-package.md) : konenMCBB: College
  Basketball Data Digestion for R
