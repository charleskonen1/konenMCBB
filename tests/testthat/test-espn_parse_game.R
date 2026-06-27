cached_game_path <- function(game_id, season = "2024-25", date = "2025-01-20") {
  rel <- file.path("data", "espn_db", season, date, game_id, "game.rds")
  candidates <- c(
    file.path(getwd(), rel),
    file.path(getwd(), "..", "..", rel),
    file.path(Sys.getenv("TESTTHAT_PKG"), rel)
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing) == 0L) return(rel)
  existing[[1L]]
}

load_parser_helpers <- function() {
  parser_candidates <- c(
    file.path(getwd(), "R", "espn_parse_game.R"),
    file.path(getwd(), "..", "..", "R", "espn_parse_game.R"),
    file.path(Sys.getenv("TESTTHAT_PKG"), "R", "espn_parse_game.R")
  )
  path_candidates <- c(
    file.path(getwd(), "R", "espn_db_paths.R"),
    file.path(getwd(), "..", "..", "R", "espn_db_paths.R"),
    file.path(Sys.getenv("TESTTHAT_PKG"), "R", "espn_db_paths.R")
  )
  parser_path <- parser_candidates[file.exists(parser_candidates)][1]
  paths_path <- path_candidates[file.exists(path_candidates)][1]
  if (!is.na(paths_path) && !exists(".espn_db_ensure_dir", mode = "function")) {
    source(paths_path, local = .GlobalEnv)
  }
  if (!is.na(parser_path) && !exists(".espn_parse_on_off", mode = "function")) {
    source(parser_path, local = .GlobalEnv)
  }
}

test_that("winprobability parser includes home/away team ids", {
  load_parser_helpers()
  path <- cached_game_path("401830225")
  skip_if_not(file.exists(path), "Cached ESPN game file not found for parser test.")

  game_obj <- readRDS(path)
  raw_n <- length(game_obj$winprobability)
  skip_if(raw_n == 0, "No raw winprobability in cached game.")

  wp <- .espn_parse_winprobability(game_obj)

  expect_equal(nrow(wp), raw_n)
  expect_true(all(c("home_team_id", "away_team_id", "home_win_percentage", "tie_percentage", "play_id") %in% names(wp)))
  expect_true(all(!is.na(wp$home_team_id)))
  expect_true(all(!is.na(wp$away_team_id)))
})

test_that("pickcenter parser preserves rows and open-favorite columns", {
  load_parser_helpers()
  path <- cached_game_path("401830225")
  skip_if_not(file.exists(path), "Cached ESPN game file not found for parser test.")

  game_obj <- readRDS(path)
  raw_n <- length(game_obj$pickcenter)
  skip_if(raw_n == 0, "No raw pickcenter in cached game.")

  pc <- .espn_parse_pickcenter(game_obj)

  expect_equal(nrow(pc), raw_n)
  expect_true(all(c("away_favorite_open", "home_favorite_open") %in% names(pc)))
})

test_that("on_off parser produces minute totals consistent with box score", {
  load_parser_helpers()
  path <- cached_game_path("401830225")
  skip_if_not(file.exists(path), "Cached ESPN game file not found for parser test.")

  game_obj <- readRDS(path)
  on_off <- .espn_parse_on_off(game_obj)
  box_players <- .espn_parse_box_players(game_obj)

  expect_gt(nrow(on_off), 10)
  expect_true("plus_minus" %in% names(on_off))
  expect_true(all(on_off$end_time >= on_off$start_time))

  minute_totals <- stats::aggregate(minutes ~ team_id + player_id, data = on_off, FUN = sum)
  box_minutes <- box_players[, c("team_id", "player_id", "minutes")]
  box_minutes$minutes <- suppressWarnings(as.numeric(box_minutes$minutes))

  joined <- merge(minute_totals, box_minutes, by = c("team_id", "player_id"), all.x = TRUE, suffixes = c("_calc", "_box"))
  joined$diff <- abs(joined$minutes_calc - joined$minutes_box)

  expect_true(all(joined$diff <= 1, na.rm = TRUE))
})
