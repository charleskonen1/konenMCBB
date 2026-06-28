## Diagnostic harness: test each exported function, report data quality.
suppressMessages(devtools::load_all("."))
espn_set_db_path("data/espn_db")

probe <- function(label, expr) {
  res <- tryCatch(expr, error = function(e) structure(list(err = conditionMessage(e)), class = "probe_err"))
  if (inherits(res, "probe_err")) {
    cat(sprintf("[FAIL] %-32s ERROR: %s\n", label, substr(res$err, 1, 90)))
    return(invisible(NULL))
  }
  if (is.null(res)) { cat(sprintf("[NULL] %-32s returned NULL\n", label)); return(invisible(NULL)) }
  if (is.character(res) && length(res) == 1) { cat(sprintf("[MSG ] %-32s '%s'\n", label, substr(res,1,60))); return(invisible(NULL)) }
  if (is.data.frame(res)) {
    nr <- nrow(res); nc <- ncol(res)
    # Count columns that are entirely NA or empty
    na_cols <- sum(sapply(res, function(col) {
      if (is.character(col)) all(is.na(col) | col == "") else all(is.na(col))
    }))
    flag <- if (nr == 0) "EMPTY" else if (na_cols > nc/2) "SPARSE" else "OK"
    cat(sprintf("[%-5s] %-32s rows=%-5d cols=%-3d all-NA-cols=%d\n", flag, label, nr, nc, na_cols))
  } else {
    cat(sprintf("[?    ] %-32s class=%s len=%d\n", label, paste(class(res),collapse=","), length(res)))
  }
  invisible(res)
}

args_env <- commandArgs(trailingOnly = TRUE)
group <- if (length(args_env) >= 1) args_env[1] else "all"
YR <- 2025  # last completed season — safe for off-season testing

if (group %in% c("espn","all")) {
  cat("\n=== ESPN (local DB + live) ===\n")
  probe("espn_teams",            espn_teams())
  probe("espn_team_games",       espn_team_games("2024-25"))
  probe("espn_player_games",     espn_player_games("2024-25"))
  probe("espn_season_box",       espn_season_box("2024-25"))
  probe("espn_season_players",   espn_season_players("2024-25"))
  probe("espn_team_season_summary", espn_team_season_summary("2024-25"))
  probe("espn_scoreboard",       espn_scoreboard(as.Date("2024-11-16")))
  probe("espn_rankings_summary", espn_rankings_summary("2024-25", "2024-11-16"))
  probe("espn_list_game_dirs",   data.frame(dir = espn_list_game_dirs("data/espn_db","2024-25")))
}

if (group %in% c("torvik1","all")) {
  cat("\n=== TORVIK group 1 ===\n")
  probe("torvik_team_ratings",   torvik_team_ratings(year = YR)); Sys.sleep(3)
  probe("torvik_four_factors",   torvik_four_factors(year = YR)); Sys.sleep(3)
  probe("torvik_shooting",       torvik_shooting(year = YR));     Sys.sleep(3)
  probe("torvik_conf_stats",     torvik_conf_stats(year = YR))
}

if (group %in% c("torvik2","all")) {
  cat("\n=== TORVIK group 2 ===\n")
  probe("get_games(Duke)",       get_games(team = "Duke", season = YR)); Sys.sleep(3)
  probe("get_super_sked",        get_super_sked(year = YR));             Sys.sleep(3)
  probe("player_stats",          player_stats(year = YR));               Sys.sleep(3)
  probe("teamBoxScore(Duke)",    teamBoxScore(team = "Duke", season = YR))
}

if (group %in% c("torvik3","all")) {
  cat("\n=== TORVIK group 3 ===\n")
  probe("current_resume",        current_resume()); Sys.sleep(3)
  probe("historic_team_results", historic_team_results(YR)); Sys.sleep(3)
  probe("dayCast",               dayCast(YR)); Sys.sleep(3)
  probe("timeMachine_ratings",   timeMachine_ratings("20250115")); Sys.sleep(3)
  probe("super_sked_with_timemachine", super_sked_with_timemachine("20250115"))
}

if (group %in% c("misc","all")) {
  cat("\n=== MISC (sportsref / ncaa / covers) ===\n")
  probe("teamList",              teamList())
  probe("teamRoster(Duke)",      teamRoster("Duke", YR))
  probe("PlayerBoxScore",        PlayerBoxScore("Cooper Flagg", YR, "Duke"))
  probe("injuryList",            injuryList())
}
