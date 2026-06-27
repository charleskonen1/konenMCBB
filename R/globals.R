# Suppress R CMD check NOTEs for variables used inside dplyr verbs
# and column names referenced without quotes.
utils::globalVariables(c(
  # dplyr .data pronoun fallback (older code paths)
  ".data",
  # espn_analytics / rankings internal functions
  "team_id", "team_name", "games", "wins", "losses",
  "points_for", "points_against", "field_goals_made", "field_goals_attempted",
  "three_point_field_goals_made", "three_point_field_goals_attempted",
  "free_throws_made", "free_throws_attempted",
  "offensive_rebounds", "defensive_rebounds", "rebounds",
  "assists", "turnovers", "steals", "blocks",
  "estimated_possessions", "home_away",
  "def_eff", "off_eff", "opp_def_avg", "opp_off_avg",
  "adj_off_eff", "adj_def_eff", "relative_rating", "relative_rating_opp",
  "game_net_eff", "expected_net_eff", "residual_net_eff",
  "game_pace", "net", "game_result", "venue_mult",
  "resume_piece", "win_pct", "adj_net_eff",
  "rank_adj_off", "rank_adj_def", "true_tempo",
  "resume_score", "home_advantage_net", "point_diff_per_game",
  "ranking_score", "ranking",
  "kill_shots", "kill_shots_allowed",
  "shot_attempts", "two_point_attempts",
  "close_range_attempts", "mid_range_attempts",
  "three_point_attempts_profile", "shot_distance_sum",
  "o_rate", "d_rate",
  # shot profile
  "points_attempted", "shot_distance", "is_three",
  "close_attempt", "three_attempt_profile", "mid_attempt",
  # on_off parser
  "period", "clock", "game_seconds", "participant1_id",
  # box player parser
  "player_id", "plus_minus",
  # espn_rankings_summary
  "ranking",
  # ncaa_pbp
  "periodNumber", "playbyplayStats", "teamId", "isHome",
  "homeScore", "visitorScore", "eventDescription",
  # ncaa_player_box
  "playerStats", "number", "firstName", "lastName", "position",
  "minutesPlayed", "year", "elig", "starter",
  "fieldGoalsMade", "fieldGoalsAttempted",
  "freeThrowsMade", "freeThrowsAttempted",
  "threePointsMade", "threePointsAttempted",
  "offensiveRebounds", "totalRebounds", "personalFouls", "blockedShots",
  "points",
  # get_games / teamBoxScore (older scrapers)
  "question", "boxScore", "Type", "backslash", "Repeat1", "Repeat2",
  # injuryList
  "Description", "Player",
  # super_sked_with_timemachine
  "team",
  # espn_season_players active_only filter
  "did_not_play",
  # dayCast derived columns
  "t1wp_n", "t2wp_n", "Favorite", "WinProb", "Pred_Score",
  "t1wp", "t2wp", "t1ppp", "t2ppp", "matchup", "prediction",
  "Matchup", "Line", "TTQ",
  # super_sked / dayCast schedule columns
  "team1", "team2", "t1pts", "t2pts", "muid", "gp",
  "t1adjt", "t2adjt", "t1adjo", "t1adjd", "t2adjo", "t2adjd",
  "Date", "venue", "confmatch",
  # timeMachine / super_sked_with_timemachine
  "adjoe", "adjde", "barthag", "adjt",
  "side", "opponent", "team_pts", "opp_pts", "game_date"
))
