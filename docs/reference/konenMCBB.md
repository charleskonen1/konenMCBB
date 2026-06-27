# konenMCBB: College Basketball Data Digestion for R

\`konenMCBB\` provides a unified interface for pulling, parsing, and
storing college basketball data from multiple sources:

\- \*\*Bart Torvik\*\* (\`barttorvik.com\`) — advanced team and player
efficiency metrics, four factors, the T-Rank power ratings, schedules,
and the Time Machine historical ratings archive. - \*\*ESPN\*\* — live
and historical game data including box scores, play-by-play, win
probability, betting lines, and on/off rotations, saved to a structured
local database. - \*\*Sports Reference\*\* (\`sports-reference.com\`) —
player game logs, rosters, and the historical team directory. -
\*\*NCAA\*\* — play-by-play and box scores direct from NCAA.com. -
\*\*Covers.com\*\* — current injury reports.

## Torvik functions

\| Function \| Description \| \|—\|—\| \| \[torvik_team_ratings()\] \|
Full T-Rank table: AdjOE, AdjDE, Barthag, WAB \| \|
\[torvik_four_factors()\] \| eFG \| \[timeMachine_ratings()\] \|
Point-in-time historical ratings snapshot \| \|
\[super_sked_with_timemachine()\] \| Schedule + pre-game ratings joined
\| \| \[get_super_sked()\] \| Full Torvik super schedule for a season \|
\| \[get_games()\] \| Game-level efficiency stats per team \| \|
\[teamBoxScore()\] \| Team box score game log \| \| \[player_stats()\]
\| Advanced player stats (BPM, ORtg, usage, etc.) \| \|
\[historic_team_results()\] \| Season team results CSV \| \|
\[current_resume()\] \| Current season team metrics tibble \| \|
\[dayCast()\] \| Today's unplayed games with Torvik lines \|

## ESPN functions

\| Function \| Description \| \|—\|—\| \| \[espn_set_db_path()\] \|
Configure local database root path \| \| \[espn_game()\] \| Fetch and
parse a single game from ESPN \| \| \[espn_process_day()\] \| Fetch all
games on a date into local DB \| \| \[espn_load_game()\] \| Load a
previously saved game from local DB \| \| \[espn_rankings_summary()\] \|
Build team rankings from local DB \|

## Sports Reference & NCAA functions

\| Function \| Description \| \|—\|—\| \| \[PlayerBoxScore()\] \| Player
game log from Sports Reference \| \| \[teamRoster()\] \| Team roster for
a season \| \| \[teamList()\] \| Historical team directory \| \|
\[ncaa_pbp()\] \| Play-by-play from NCAA.com \| \| \[ncaa_player_box()\]
\| Player box score from NCAA.com \|

## Injury data

\| Function \| Description \| \|—\|—\| \| \[injuryList()\] \| Current
NCAAB injury report from Covers.com \|

## ESPN local database

The ESPN pipeline stores data as a nested folder structure: “\` espn_db/
2024-25/ 2024-11-11/ 401700444/ raw.json \# raw ESPN API response
game.rds \# parsed list object box_team_stats.rds box_players.rds
plays.rds winprobability.rds pickcenter.rds on_off.rds leaders.rds
game_info.rds “\` Configure the root with \`espn_set_db_path()\` or
\`options(konenMCBB.espn_db_path = "/path/to/espn_db")\`.

## PostgreSQL loading

The package ships with \`load_espn_db_to_postgres.R\` — a standalone
script that bulk-loads all local RDS files into a PostgreSQL database
using the schema defined in \`schema.sql\`. This powers the public stats
website.
