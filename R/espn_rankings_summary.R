#' Build Incremental ESPN Season Rankings Summary
#'
#' Creates a daily team rankings table from the local ESPN game database.
#' If a rankings state file exists for the previous day, the function reuses
#' that state and only processes newly available day folders.
#'
#' @param season Season label (for example, `"2024-25"`).
#' @param date Target date (`Date` or `"YYYY-MM-DD"`). Rankings include games
#'   up to and including this date.
#' @param base_path Base ESPN DB path. Defaults to `options(konenMCBB.espn_db_path)`
#'   fallback used by internal helpers.
#' @param use_incremental If `TRUE`, attempt to load the previous day's saved
#'   rankings state and append only newer days.
#' @param save_state If `TRUE`, write a state file at
#'   `<base_path>/<season>/rankings/<date>.rds`.
#'
#' @return A tibble with team-level cumulative stats and ranks.
#' @export
espn_rankings_summary <- function(
  season,
  date = Sys.Date(),
  base_path = .espn_db_base(),
  use_incremental = TRUE,
  save_state = TRUE
) {
  season <- as.character(season)
  target_date <- as.Date(date)
  season_dir <- .espn_db_season_dir(base_path, season)
  if (!dir.exists(season_dir)) return(tibble::tibble())

  day_dirs <- list.dirs(season_dir, full.names = TRUE, recursive = FALSE)
  day_names <- basename(day_dirs)
  day_dates <- suppressWarnings(as.Date(day_names))
  keep <- !is.na(day_dates) & day_dates <= target_date
  day_dirs <- day_dirs[keep]
  day_dates <- day_dates[keep]
  if (length(day_dirs) == 0L) return(tibble::tibble())

  ord <- order(day_dates)
  day_dirs <- day_dirs[ord]
  day_dates <- day_dates[ord]

  rankings_dir <- .espn_db_ensure_dir(file.path(season_dir, "rankings"))
  team_games <- tibble::tibble()
  processed_dates <- as.Date(character(0))
  state_date <- NULL

  if (isTRUE(use_incremental)) {
    prev_date <- target_date - 1
    prev_path <- file.path(rankings_dir, paste0(format(prev_date, "%Y-%m-%d"), ".rds"))
    if (file.exists(prev_path)) {
      st <- readRDS(prev_path)
      if (is.list(st) && "team_games" %in% names(st) && "processed_dates" %in% names(st)) {
        team_games <- st$team_games
        processed_dates <- as.Date(st$processed_dates)
        state_date <- prev_date
        required_cols <- names(.espn_empty_team_games())
        if (!all(required_cols %in% names(team_games))) {
          # Schema upgraded; rebuild from scratch for correctness.
          team_games <- .espn_empty_team_games()
          processed_dates <- as.Date(character(0))
          state_date <- NULL
        }
      }
    }
  }

  if (nrow(team_games) == 0L) {
    team_games <- .espn_empty_team_games()
  }

  pending <- day_dates[!day_dates %in% processed_dates]
  if (!is.null(state_date)) pending <- pending[pending > state_date]

  for (d in pending) {
    d_date <- as.Date(d, origin = "1970-01-01")
    day_rows <- .espn_rankings_day_games(file.path(season_dir, format(d_date, "%Y-%m-%d")), d_date)
    if (nrow(day_rows) > 0L) {
      team_games <- dplyr::bind_rows(team_games, day_rows)
    }
    processed_dates <- sort(unique(c(processed_dates, d_date)))
  }

  rankings <- .espn_rankings_from_team_games(team_games)
  rankings <- dplyr::arrange(rankings, ranking)

  if (isTRUE(save_state)) {
    out_path <- file.path(rankings_dir, paste0(format(target_date, "%Y-%m-%d"), ".rds"))
    saveRDS(list(
      date = as.character(target_date),
      season = season,
      processed_dates = as.character(processed_dates),
      team_games = team_games,
      rankings = rankings
    ), out_path)
  }

  rankings
}

.espn_empty_team_games <- function() {
  tibble::tibble(
    date = as.Date(character()),
    game_id = character(),
    team_id = character(),
    team_name = character(),
    opp_team_id = character(),
    home_away = character(),
    games = numeric(),
    wins = numeric(),
    losses = numeric(),
    points_for = numeric(),
    points_against = numeric(),
    field_goals_made = numeric(),
    field_goals_attempted = numeric(),
    three_point_field_goals_made = numeric(),
    three_point_field_goals_attempted = numeric(),
    free_throws_made = numeric(),
    free_throws_attempted = numeric(),
    offensive_rebounds = numeric(),
    defensive_rebounds = numeric(),
    rebounds = numeric(),
    assists = numeric(),
    turnovers = numeric(),
    steals = numeric(),
    blocks = numeric(),
    estimated_possessions = numeric(),
    kill_shots = numeric(),
    kill_shots_allowed = numeric(),
    shot_attempts = numeric(),
    two_point_attempts = numeric(),
    close_range_attempts = numeric(),
    mid_range_attempts = numeric(),
    three_point_attempts_profile = numeric(),
    shot_distance_sum = numeric()
  )
}

.espn_rankings_num_col <- function(df, candidates) {
  nm <- intersect(candidates, names(df))
  if (length(nm) == 0L) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[nm[[1L]]]]))
}

.espn_safe_slope <- function(x, y, min_n = 3L) {
  keep <- !(is.na(x) | is.na(y))
  if (sum(keep) < min_n) return(NA_real_)
  if (length(unique(x[keep])) < 2L) return(NA_real_)
  fit <- try(stats::lm(y[keep] ~ x[keep]), silent = TRUE)
  if (inherits(fit, "try-error")) return(NA_real_)
  coef <- stats::coef(fit)
  if (length(coef) < 2L) return(NA_real_)
  as.numeric(coef[2L])
}

.espn_rankings_split_made_attempted <- function(df, source_col) {
  if (!(source_col %in% names(df))) return(list(made = rep(NA_real_, nrow(df)), attempted = rep(NA_real_, nrow(df))))
  p <- stringr::str_split_fixed(as.character(df[[source_col]]), "-", 2)
  list(made = suppressWarnings(as.numeric(p[, 1L])), attempted = suppressWarnings(as.numeric(p[, 2L])))
}

.espn_rankings_name_map <- function(game_obj) {
  comps <- .get_in(game_obj, "header", "competitions")
  if (is.null(comps) || length(comps) == 0L) return(tibble::tibble(team_id = character(), team_name = character()))
  competitors <- comps[[1]]$competitors
  if (is.null(competitors) || length(competitors) == 0L) return(tibble::tibble(team_id = character(), team_name = character()))
  purrr::map_dfr(competitors, function(x) {
    tibble::tibble(
      team_id = as.character(.null_or(.get_in(x, "team", "id"), NA_character_)),
      team_name = as.character(.null_or(.get_in(x, "team", "displayName"), .null_or(.get_in(x, "team", "shortDisplayName"), NA_character_)))
    )
  })
}

.espn_rankings_shot_profile <- function(plays_df) {
  if (is.null(plays_df) || nrow(plays_df) == 0L) {
    return(tibble::tibble(
      team_id = character(),
      shot_attempts = numeric(),
      two_point_attempts = numeric(),
      close_range_attempts = numeric(),
      mid_range_attempts = numeric(),
      three_point_attempts_profile = numeric(),
      shot_distance_sum = numeric()
    ))
  }

  dist <- if ("shot_distance" %in% names(plays_df)) suppressWarnings(as.numeric(plays_df$shot_distance)) else rep(NA_real_, nrow(plays_df))
  if (all(is.na(dist)) && "shotDistance" %in% names(plays_df)) dist <- suppressWarnings(as.numeric(plays_df$shotDistance))
  if (all(is.na(dist)) && all(c("x_coordinate", "y_coordinate") %in% names(plays_df))) {
    x <- suppressWarnings(as.numeric(plays_df$x_coordinate))
    y <- suppressWarnings(as.numeric(plays_df$y_coordinate))
    dist <- ifelse(!is.na(x) & !is.na(y), sqrt((x - 25)^2 + y^2), NA_real_)
  }

  is_shot <- rep(FALSE, nrow(plays_df))
  if ("shooting_play" %in% names(plays_df)) {
    is_shot <- as.logical(plays_df$shooting_play)
  } else {
    is_shot <- rep(FALSE, nrow(plays_df))
  }

  pts_att <- if ("points_attempted" %in% names(plays_df)) suppressWarnings(as.numeric(plays_df$points_attempted)) else rep(NA_real_, nrow(plays_df))
  team_id <- as.character(plays_df$team_id)

  shot_df <- tibble::tibble(
    team_id = team_id,
    is_shot = is_shot,
    points_attempted = pts_att,
    shot_distance = dist
  ) |>
    dplyr::filter(is_shot %in% TRUE, !is.na(team_id), nzchar(team_id)) |>
    dplyr::mutate(
      is_three = !is.na(points_attempted) & points_attempted >= 3,
      # Close shot: any 2PT attempt within 4 feet.
      close_attempt = dplyr::if_else(!is.na(shot_distance) & shot_distance <= 4 & !is_three, 1, 0),
      three_attempt_profile = dplyr::if_else(is_three | (!is.na(shot_distance) & shot_distance >= 22), 1, 0),
      mid_attempt = dplyr::if_else(close_attempt == 0 & three_attempt_profile == 0, 1, 0)
    )

  shot_df |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(
      shot_attempts = dplyr::n(),
      two_point_attempts = sum(!is_three, na.rm = TRUE),
      close_range_attempts = sum(close_attempt, na.rm = TRUE),
      mid_range_attempts = sum(mid_attempt, na.rm = TRUE),
      three_point_attempts_profile = sum(three_attempt_profile, na.rm = TRUE),
      shot_distance_sum = sum(shot_distance, na.rm = TRUE),
      .groups = "drop"
    )
}

.espn_rankings_kill_shots <- function(plays_df, box_df) {
  if (is.null(plays_df) || nrow(plays_df) == 0L || is.null(box_df) || nrow(box_df) != 2L) {
    return(tibble::tibble(team_id = character(), kill_shots = numeric(), kill_shots_allowed = numeric()))
  }
  if (!all(c("home_away", "team_id") %in% names(box_df))) {
    return(tibble::tibble(team_id = character(), kill_shots = numeric(), kill_shots_allowed = numeric()))
  }
  home_team <- as.character(box_df$team_id[box_df$home_away == "home"])[1]
  away_team <- as.character(box_df$team_id[box_df$home_away == "away"])[1]
  if (is.na(home_team) || is.na(away_team)) {
    return(tibble::tibble(team_id = character(), kill_shots = numeric(), kill_shots_allowed = numeric()))
  }

  p <- plays_df
  if (!all(c("home_score", "away_score") %in% names(p))) {
    return(tibble::tibble(team_id = c(home_team, away_team), kill_shots = 0, kill_shots_allowed = 0))
  }
  seq_num <- suppressWarnings(as.numeric(p$sequence_number))
  ord <- order(seq_num, na.last = TRUE)
  p <- p[ord, , drop = FALSE]
  hs <- suppressWarnings(as.numeric(p$home_score))
  as <- suppressWarnings(as.numeric(p$away_score))
  hs[is.na(hs)] <- 0
  as[is.na(as)] <- 0
  dh <- pmax(0, hs - dplyr::lag(hs, default = 0))
  da <- pmax(0, as - dplyr::lag(as, default = 0))

  run_h <- 0
  run_a <- 0
  ks_h <- 0
  ks_a <- 0
  for (i in seq_along(dh)) {
    prev_h <- run_h
    prev_a <- run_a
    if (dh[i] > 0 && da[i] == 0) {
      run_h <- run_h + dh[i]
      run_a <- 0
    } else if (da[i] > 0 && dh[i] == 0) {
      run_a <- run_a + da[i]
      run_h <- 0
    } else if (dh[i] > 0 && da[i] > 0) {
      run_h <- dh[i]
      run_a <- da[i]
    }
    if (prev_h < 10 && run_h >= 10) ks_h <- ks_h + 1
    if (prev_a < 10 && run_a >= 10) ks_a <- ks_a + 1
  }

  tibble::tibble(
    team_id = c(home_team, away_team),
    kill_shots = c(ks_h, ks_a),
    kill_shots_allowed = c(ks_a, ks_h)
  )
}

.espn_rankings_day_games <- function(day_dir, day_date) {
  game_dirs <- list.dirs(day_dir, full.names = TRUE, recursive = FALSE)
  if (length(game_dirs) == 0L) return(.espn_empty_team_games())

  out <- purrr::map_dfr(game_dirs, function(gd) {
    game_id <- basename(gd)
    box_path <- file.path(gd, "box_team_stats.rds")
    game_path <- file.path(gd, "game.rds")
    plays_path <- file.path(gd, "plays.rds")
    if (!file.exists(box_path)) return(tibble::tibble())
    box <- readRDS(box_path)
    if (nrow(box) != 2L || !all(c("team_id", "home_away") %in% names(box))) return(tibble::tibble())

    fgm <- .espn_rankings_num_col(box, c("field_goals_made", "fieldGoalsMade"))
    fga <- .espn_rankings_num_col(box, c("field_goals_attempted", "fieldGoalsAttempted"))
    fg3m <- .espn_rankings_num_col(box, c("three_point_field_goals_made", "threePointFieldGoalsMade"))
    fg3a <- .espn_rankings_num_col(box, c("three_point_field_goals_attempted", "threePointFieldGoalsAttempted"))
    ftm <- .espn_rankings_num_col(box, c("free_throws_made", "freeThrowsMade"))
    fta <- .espn_rankings_num_col(box, c("free_throws_attempted", "freeThrowsAttempted"))

    if (all(is.na(fgm))) {
      s <- .espn_rankings_split_made_attempted(box, "fieldGoalsMade-fieldGoalsAttempted")
      fgm <- s$made
      fga <- s$attempted
    }
    if (all(is.na(fg3m))) {
      s <- .espn_rankings_split_made_attempted(box, "threePointFieldGoalsMade-threePointFieldGoalsAttempted")
      fg3m <- s$made
      fg3a <- s$attempted
    }
    if (all(is.na(ftm))) {
      s <- .espn_rankings_split_made_attempted(box, "freeThrowsMade-freeThrowsAttempted")
      ftm <- s$made
      fta <- s$attempted
    }

    pts <- .espn_rankings_num_col(box, c("points"))
    pts_fallback <- (2 * (fgm - fg3m)) + (3 * fg3m) + ftm
    pts <- dplyr::if_else(is.na(pts), pts_fallback, pts)

    oreb <- .espn_rankings_num_col(box, c("offensiveRebounds", "offensive_rebounds"))
    dreb <- .espn_rankings_num_col(box, c("defensiveRebounds", "defensive_rebounds"))
    reb <- .espn_rankings_num_col(box, c("totalRebounds", "rebounds"))
    ast <- .espn_rankings_num_col(box, c("assists"))
    tov <- .espn_rankings_num_col(box, c("turnovers"))
    stl <- .espn_rankings_num_col(box, c("steals"))
    blk <- .espn_rankings_num_col(box, c("blocks"))
    poss <- fga - oreb + tov + (0.44 * fta)

    opp_pts <- rev(pts)
    opp_team <- rev(as.character(box$team_id))
    win <- ifelse(pts > opp_pts, 1, 0)
    loss <- ifelse(pts < opp_pts, 1, 0)

    nm <- tibble::tibble(team_id = as.character(box$team_id), team_name = as.character(box$team_id))
    if (file.exists(game_path)) {
      nm2 <- .espn_rankings_name_map(readRDS(game_path))
      if (nrow(nm2) > 0L) nm <- dplyr::left_join(nm, nm2, by = "team_id")
    }
    nm$team_name <- dplyr::coalesce(nm$team_name.y, nm$team_name.x)

    shot_prof <- tibble::tibble(team_id = as.character(box$team_id))
    kill_prof <- tibble::tibble(team_id = as.character(box$team_id), kill_shots = 0, kill_shots_allowed = 0)
    plays_df <- NULL
    if (file.exists(plays_path)) {
      plays_df <- readRDS(plays_path)
      shot_prof <- .espn_rankings_shot_profile(plays_df)
      kill_prof <- .espn_rankings_kill_shots(plays_df, box)
    }

    tibble::tibble(
      date = as.Date(day_date),
      game_id = as.character(game_id),
      team_id = as.character(box$team_id),
      team_name = as.character(nm$team_name),
      opp_team_id = as.character(opp_team),
      home_away = as.character(box$home_away),
      games = 1,
      wins = as.numeric(win),
      losses = as.numeric(loss),
      points_for = pts,
      points_against = opp_pts,
      field_goals_made = fgm,
      field_goals_attempted = fga,
      three_point_field_goals_made = fg3m,
      three_point_field_goals_attempted = fg3a,
      free_throws_made = ftm,
      free_throws_attempted = fta,
      offensive_rebounds = oreb,
      defensive_rebounds = dreb,
      rebounds = reb,
      assists = ast,
      turnovers = tov,
      steals = stl,
      blocks = blk,
      estimated_possessions = poss
    ) |>
      dplyr::left_join(shot_prof, by = "team_id") |>
      dplyr::left_join(kill_prof, by = "team_id") |>
      dplyr::mutate(
        shot_attempts = dplyr::coalesce(shot_attempts, 0),
        two_point_attempts = dplyr::coalesce(two_point_attempts, 0),
        close_range_attempts = dplyr::coalesce(close_range_attempts, 0),
        mid_range_attempts = dplyr::coalesce(mid_range_attempts, 0),
        three_point_attempts_profile = dplyr::coalesce(three_point_attempts_profile, 0),
        shot_distance_sum = dplyr::coalesce(shot_distance_sum, 0),
        kill_shots = dplyr::coalesce(kill_shots, 0),
        kill_shots_allowed = dplyr::coalesce(kill_shots_allowed, 0)
      )
  })

  out
}

.espn_relative_snapshot <- function(team_games) {
  if (nrow(team_games) == 0L) return(tibble::tibble(team_id = character(), relative_rating = numeric()))
  s <- team_games |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(
      points_for = sum(points_for, na.rm = TRUE),
      points_against = sum(points_against, na.rm = TRUE),
      estimated_possessions = sum(estimated_possessions, na.rm = TRUE),
      .groups = "drop"
    )
  s$off_eff <- ifelse(s$estimated_possessions > 0, (s$points_for / s$estimated_possessions) * 100, NA_real_)
  s$def_eff <- ifelse(s$estimated_possessions > 0, (s$points_against / s$estimated_possessions) * 100, NA_real_)
  league_avg_eff <- ifelse(sum(s$estimated_possessions, na.rm = TRUE) > 0,
    (sum(s$points_for, na.rm = TRUE) / sum(s$estimated_possessions, na.rm = TRUE)) * 100,
    NA_real_
  )
  opp_map <- s[, c("team_id", "off_eff", "def_eff")]
  g <- team_games |>
    dplyr::left_join(opp_map, by = c("opp_team_id" = "team_id"), suffix = c("", "_opp"))
  opp_strength <- g |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(
      opp_def_avg = stats::weighted.mean(def_eff, w = pmax(estimated_possessions, 1), na.rm = TRUE),
      opp_off_avg = stats::weighted.mean(off_eff, w = pmax(estimated_possessions, 1), na.rm = TRUE),
      .groups = "drop"
    )
  s <- s |>
    dplyr::left_join(opp_strength, by = "team_id") |>
    dplyr::mutate(
      adj_off_eff = dplyr::if_else(!is.na(opp_def_avg) & opp_def_avg > 0 & !is.na(league_avg_eff), off_eff * (league_avg_eff / opp_def_avg), NA_real_),
      adj_def_eff = dplyr::if_else(!is.na(opp_off_avg) & opp_off_avg > 0 & !is.na(league_avg_eff), def_eff * (league_avg_eff / opp_off_avg), NA_real_),
      o_rate = adj_off_eff - league_avg_eff,
      d_rate = league_avg_eff - adj_def_eff,
      relative_rating = o_rate + d_rate
    ) |>
    dplyr::select(team_id, relative_rating)
  s
}

.espn_rankings_from_team_games <- function(team_games) {
  if (nrow(team_games) == 0L) return(tibble::tibble())

  cumulative <- team_games |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(
      team_name = dplyr::last(stats::na.omit(team_name)),
      games = sum(games, na.rm = TRUE),
      wins = sum(wins, na.rm = TRUE),
      losses = sum(losses, na.rm = TRUE),
      points_for = sum(points_for, na.rm = TRUE),
      points_against = sum(points_against, na.rm = TRUE),
      field_goals_made = sum(field_goals_made, na.rm = TRUE),
      field_goals_attempted = sum(field_goals_attempted, na.rm = TRUE),
      three_point_field_goals_made = sum(three_point_field_goals_made, na.rm = TRUE),
      three_point_field_goals_attempted = sum(three_point_field_goals_attempted, na.rm = TRUE),
      free_throws_made = sum(free_throws_made, na.rm = TRUE),
      free_throws_attempted = sum(free_throws_attempted, na.rm = TRUE),
      offensive_rebounds = sum(offensive_rebounds, na.rm = TRUE),
      defensive_rebounds = sum(defensive_rebounds, na.rm = TRUE),
      rebounds = sum(rebounds, na.rm = TRUE),
      assists = sum(assists, na.rm = TRUE),
      turnovers = sum(turnovers, na.rm = TRUE),
      steals = sum(steals, na.rm = TRUE),
      blocks = sum(blocks, na.rm = TRUE),
      estimated_possessions = sum(estimated_possessions, na.rm = TRUE),
      kill_shots = sum(kill_shots, na.rm = TRUE),
      kill_shots_allowed = sum(kill_shots_allowed, na.rm = TRUE),
      shot_attempts = sum(shot_attempts, na.rm = TRUE),
      two_point_attempts = sum(two_point_attempts, na.rm = TRUE),
      close_range_attempts = sum(close_range_attempts, na.rm = TRUE),
      mid_range_attempts = sum(mid_range_attempts, na.rm = TRUE),
      three_point_attempts_profile = sum(three_point_attempts_profile, na.rm = TRUE),
      shot_distance_sum = sum(shot_distance_sum, na.rm = TRUE),
      .groups = "drop"
    )

  c <- cumulative
  c$win_pct <- ifelse(c$games > 0, c$wins / c$games, NA_real_)
  c$points_per_game <- ifelse(c$games > 0, c$points_for / c$games, NA_real_)
  c$opp_points_per_game <- ifelse(c$games > 0, c$points_against / c$games, NA_real_)
  c$point_diff_per_game <- ifelse(c$games > 0, (c$points_for - c$points_against) / c$games, NA_real_)

  c$fg_pct <- ifelse(c$field_goals_attempted > 0, c$field_goals_made / c$field_goals_attempted, NA_real_)
  c$threep_pct <- ifelse(c$three_point_field_goals_attempted > 0, c$three_point_field_goals_made / c$three_point_field_goals_attempted, NA_real_)
  c$ft_pct <- ifelse(c$free_throws_attempted > 0, c$free_throws_made / c$free_throws_attempted, NA_real_)

  c$off_eff <- ifelse(c$estimated_possessions > 0, (c$points_for / c$estimated_possessions) * 100, NA_real_)
  c$def_eff <- ifelse(c$estimated_possessions > 0, (c$points_against / c$estimated_possessions) * 100, NA_real_)
  c$net_eff <- c$off_eff - c$def_eff
  c$eff <- c$off_eff

  c$pace <- ifelse(c$games > 0, c$estimated_possessions / c$games, NA_real_)
  c$ftar <- ifelse(c$estimated_possessions > 0, (c$free_throws_attempted / c$estimated_possessions) * 100, NA_real_)
  c$fgar <- ifelse(c$estimated_possessions > 0, (c$field_goals_attempted / c$estimated_possessions) * 100, NA_real_)
  c$threepar <- ifelse(c$estimated_possessions > 0, (c$three_point_field_goals_attempted / c$estimated_possessions) * 100, NA_real_)
  c$pct_3pa <- ifelse(c$field_goals_attempted > 0, c$three_point_field_goals_attempted / c$field_goals_attempted, NA_real_)

  c$pct_close_range <- ifelse(c$shot_attempts > 0, c$close_range_attempts / c$shot_attempts, NA_real_)
  c$close_shot_rate_2pt <- ifelse(c$two_point_attempts > 0, c$close_range_attempts / c$two_point_attempts, NA_real_)
  c$pct_mid_range <- ifelse(c$shot_attempts > 0, c$mid_range_attempts / c$shot_attempts, NA_real_)
  c$pct_three_profile <- ifelse(c$shot_attempts > 0, c$three_point_attempts_profile / c$shot_attempts, NA_real_)
  c$avg_shot_distance <- ifelse(c$shot_attempts > 0, c$shot_distance_sum / c$shot_attempts, NA_real_)
  c$prox <- ifelse(c$pct_three_profile > 0 | c$pct_mid_range > 0 | c$pct_close_range > 0,
    (1 * c$pct_close_range) + (2 * c$pct_mid_range) + (3 * c$pct_three_profile),
    NA_real_
  )

  c$kill_shots_per_game <- ifelse(c$games > 0, c$kill_shots / c$games, NA_real_)
  c$kill_shots_allowed_per_game <- ifelse(c$games > 0, c$kill_shots_allowed / c$games, NA_real_)
  c$kill_shots_margin_per_game <- c$kill_shots_per_game - c$kill_shots_allowed_per_game
  c$total_kill_shots <- c$kill_shots
  c$total_kill_shots_allowed <- c$kill_shots_allowed

  # One-pass opponent-adjusted efficiencies.
  league_avg_eff <- ifelse(sum(c$estimated_possessions, na.rm = TRUE) > 0,
    (sum(c$points_for, na.rm = TRUE) / sum(c$estimated_possessions, na.rm = TRUE)) * 100,
    NA_real_
  )

  opp_map <- c[, c("team_id", "off_eff", "def_eff")]
  games_with_opp <- team_games |>
    dplyr::left_join(opp_map, by = c("opp_team_id" = "team_id"), suffix = c("", "_opp"))

  opp_strength <- games_with_opp |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(
      opp_def_avg = stats::weighted.mean(def_eff, w = pmax(estimated_possessions, 1), na.rm = TRUE),
      opp_off_avg = stats::weighted.mean(off_eff, w = pmax(estimated_possessions, 1), na.rm = TRUE),
      .groups = "drop"
    )

  c <- c |>
    dplyr::left_join(opp_strength, by = "team_id") |>
    dplyr::mutate(
      adj_off_eff = dplyr::if_else(!is.na(opp_def_avg) & opp_def_avg > 0 & !is.na(league_avg_eff), off_eff * (league_avg_eff / opp_def_avg), NA_real_),
      adj_def_eff = dplyr::if_else(!is.na(opp_off_avg) & opp_off_avg > 0 & !is.na(league_avg_eff), def_eff * (league_avg_eff / opp_off_avg), NA_real_),
      adj_net_eff = adj_off_eff - adj_def_eff,
      sos_eff = (opp_off_avg + opp_def_avg) / 2
    )

  # Relative ratings (interpretable as points per 100 better than average, with one-pass adj).
  c$o_rate <- c$adj_off_eff - league_avg_eff
  c$d_rate <- league_avg_eff - c$adj_def_eff
  c$relative_rating <- c$o_rate + c$d_rate

  # Opponent-adjust and pace-adjust: slopes of residual performance.
  team_strength <- c[, c("team_id", "relative_rating", "pace")]
  games_aug <- team_games |>
    dplyr::left_join(team_strength, by = "team_id") |>
    dplyr::left_join(team_strength, by = c("opp_team_id" = "team_id"), suffix = c("", "_opp")) |>
    dplyr::mutate(
      game_net_eff = ifelse(estimated_possessions > 0, ((points_for - points_against) / estimated_possessions) * 100, NA_real_),
      expected_net_eff = relative_rating - relative_rating_opp,
      residual_net_eff = game_net_eff - expected_net_eff,
      game_pace = estimated_possessions
    )

  opp_adj_tbl <- games_aug |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(
      opponent_adjust = .espn_safe_slope(relative_rating_opp, residual_net_eff, min_n = 3L),
      .groups = "drop"
    )
  pace_adj_tbl <- games_aug |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(
      pace_adjust = .espn_safe_slope(game_pace, residual_net_eff, min_n = 3L),
      true_tempo = ifelse(sum(!is.na(game_pace)) > 0, mean(game_pace, na.rm = TRUE), NA_real_),
      .groups = "drop"
    )
  c <- c |>
    dplyr::left_join(opp_adj_tbl, by = "team_id") |>
    dplyr::left_join(pace_adj_tbl, by = "team_id")

  # Home rank proxy: net efficiency split (home - away).
  home_tbl <- games_aug |>
    dplyr::group_by(team_id, home_away) |>
    dplyr::summarise(net = mean(game_net_eff, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = home_away, values_from = net, values_fill = NA_real_)
  if (!("home" %in% names(home_tbl))) home_tbl$home <- NA_real_
  if (!("away" %in% names(home_tbl))) home_tbl$away <- NA_real_
  if (!("neutral" %in% names(home_tbl))) home_tbl$neutral <- NA_real_
  home_tbl$home_advantage_net <- home_tbl$home - home_tbl$away
  c <- c |> dplyr::left_join(home_tbl[, c("team_id", "home_advantage_net")], by = "team_id")

  # Resume rank proxy: quality of wins/losses by opponent strength + venue multipliers.
  resume_tbl <- games_aug |>
    dplyr::mutate(
      venue_mult = dplyr::case_when(
        home_away == "away" ~ 1.1,
        home_away == "home" ~ 0.9,
        TRUE ~ 1.0
      ),
      opp_strength = dplyr::if_else(!is.na(relative_rating_opp), pmax(0.3, 0.5 + (relative_rating_opp / 30)), 0.5),
      game_result = dplyr::if_else(points_for > points_against, 1, dplyr::if_else(points_for < points_against, -1, 0)),
      resume_piece = game_result * opp_strength * venue_mult
    ) |>
    dplyr::group_by(team_id) |>
    dplyr::summarise(resume_score = mean(resume_piece, na.rm = TRUE), .groups = "drop")
  c <- c |> dplyr::left_join(resume_tbl, by = "team_id")

  # 30-day change in relative rating.
  latest_date <- suppressWarnings(max(as.Date(team_games$date), na.rm = TRUE))
  if (!is.na(latest_date) && !is.infinite(as.numeric(latest_date))) {
    cutoff <- latest_date - 30
    snap_30 <- .espn_relative_snapshot(team_games[as.Date(team_games$date) <= cutoff, , drop = FALSE])
    names(snap_30)[names(snap_30) == "relative_rating"] <- "relative_rating_30d_ago"
    c <- c |> dplyr::left_join(snap_30, by = "team_id")
    c$change_30d <- c$relative_rating - c$relative_rating_30d_ago
  } else {
    c$relative_rating_30d_ago <- NA_real_
    c$change_30d <- NA_real_
  }

  c <- c |>
    dplyr::mutate(
      rank_win_pct = dplyr::dense_rank(dplyr::desc(win_pct)),
      relative_ranking = dplyr::dense_rank(dplyr::desc(relative_rating)),
      rank_adj_net = dplyr::dense_rank(dplyr::desc(adj_net_eff)),
      rank_adj_off = dplyr::dense_rank(dplyr::desc(adj_off_eff)),
      rank_adj_def = dplyr::dense_rank(adj_def_eff),
      off_rank = rank_adj_off,
      def_rank = rank_adj_def,
      tempo_rank = dplyr::dense_rank(dplyr::desc(true_tempo)),
      resume_rank = dplyr::dense_rank(dplyr::desc(resume_score)),
      home_rank = dplyr::dense_rank(dplyr::desc(home_advantage_net)),
      ranking_score = (win_pct * 100) + dplyr::coalesce(relative_rating, 0) + (point_diff_per_game / 10),
      ranking = dplyr::dense_rank(dplyr::desc(ranking_score)),
      injury_rank = NA_real_,
      roster_rank = NA_real_
    ) |>
    dplyr::arrange(ranking, dplyr::desc(ranking_score))

  c
}
