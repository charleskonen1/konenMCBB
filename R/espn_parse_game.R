# Internal: parse game object (ESPN summary list) into dataframes. No new columns beyond API.

.null_or <- function(x, y) if (is.null(x)) y else x

# Safe nested list access: .get_in(obj, "a", "b", "c") => obj$a$b$c or NULL
.get_in <- function(x, ...) {
  for (n in list(...)) {
    if (is.null(x)) return(NULL)
    x <- x[[n]]
  }
  x
}

# Convert stat values to numeric (safe for character/integer/double).
.espn_as_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

# Parse made-attempted string columns into numeric made/attempted columns.
.espn_split_made_attempted <- function(df, source_col, out_prefix) {
  if (!(source_col %in% names(df))) return(df)
  vals <- as.character(df[[source_col]])
  parts <- stringr::str_split_fixed(vals, "-", 2)
  if (ncol(parts) < 2L) return(df)
  df[[paste0(out_prefix, "_made")]] <- .espn_as_numeric(parts[, 1L])
  df[[paste0(out_prefix, "_attempted")]] <- .espn_as_numeric(parts[, 2L])
  df
}

# Parse minutes that may be "MM" or "MM:SS".
.espn_minutes_to_numeric <- function(x) {
  x <- as.character(x)
  out <- rep(NA_real_, length(x))
  is_mmss <- !is.na(x) & stringr::str_detect(x, "^[0-9]+:[0-9]{2}$")
  if (any(is_mmss)) {
    parts <- stringr::str_split_fixed(x[is_mmss], ":", 2)
    out[is_mmss] <- .espn_as_numeric(parts[, 1L]) + (.espn_as_numeric(parts[, 2L]) / 60)
  }
  not_mmss <- !is.na(x) & !is_mmss
  if (any(not_mmss)) out[not_mmss] <- .espn_as_numeric(x[not_mmss])
  out
}

.espn_safe_div <- function(num, den) {
  dplyr::if_else(!is.na(den) & den > 0, num / den, NA_real_)
}

# Add derived advanced metrics using parsed box-score columns.
.espn_add_box_advanced <- function(df, minutes_col = NULL) {
  if (nrow(df) == 0L) return(df)

  if (!("field_goals_made" %in% names(df))) return(df)
  if (!("field_goals_attempted" %in% names(df))) return(df)
  if (!("three_point_field_goals_made" %in% names(df))) return(df)
  if (!("three_point_field_goals_attempted" %in% names(df))) return(df)
  if (!("free_throws_made" %in% names(df))) return(df)
  if (!("free_throws_attempted" %in% names(df))) return(df)

  if (!("points" %in% names(df))) {
    df$points <- (2 * (df$field_goals_made - df$three_point_field_goals_made)) +
      (3 * df$three_point_field_goals_made) + df$free_throws_made
  } else {
    points_num <- .espn_as_numeric(df$points)
    df$points <- dplyr::if_else(is.na(points_num),
      (2 * (df$field_goals_made - df$three_point_field_goals_made)) +
        (3 * df$three_point_field_goals_made) + df$free_throws_made,
      points_num
    )
  }

  df$two_point_field_goals_made <- df$field_goals_made - df$three_point_field_goals_made
  df$two_point_field_goals_attempted <- df$field_goals_attempted - df$three_point_field_goals_attempted

  # Directly-computable metrics aligned to requested nomenclature.
  df$fg_pct <- .espn_safe_div(df$field_goals_made, df$field_goals_attempted)
  df$threep_pct <- .espn_safe_div(df$three_point_field_goals_made, df$three_point_field_goals_attempted)
  df$ft_pct <- .espn_safe_div(df$free_throws_made, df$free_throws_attempted)

  df$effective_field_goal_pct <- .espn_safe_div(df$field_goals_made + (0.5 * df$three_point_field_goals_made), df$field_goals_attempted)
  df$true_shooting_pct <- .espn_safe_div(df$points, 2 * (df$field_goals_attempted + (0.44 * df$free_throws_attempted)))
  df$free_throw_rate <- .espn_safe_div(df$free_throws_attempted, df$field_goals_attempted)
  df$three_point_attempt_rate <- .espn_safe_div(df$three_point_field_goals_attempted, df$field_goals_attempted)

  if ("assists" %in% names(df) && "turnovers" %in% names(df)) {
    assists_num <- .espn_as_numeric(df$assists)
    turnovers_num <- .espn_as_numeric(df$turnovers)
    df$assist_to_turnover_ratio <- .espn_safe_div(assists_num, turnovers_num)
  }

  offensive_rebounds <- if ("offensiveRebounds" %in% names(df)) .espn_as_numeric(df$offensiveRebounds) else if ("offensive_rebounds" %in% names(df)) .espn_as_numeric(df$offensive_rebounds) else NA_real_
  turnovers <- if ("turnovers" %in% names(df)) .espn_as_numeric(df$turnovers) else NA_real_
  df$estimated_possessions <- df$field_goals_attempted - offensive_rebounds + turnovers + (0.44 * df$free_throws_attempted)
  df$points_per_estimated_possession <- .espn_safe_div(df$points, df$estimated_possessions)
  df$points_per_shot_attempt <- .espn_safe_div(df$points, df$field_goals_attempted)
  df$pace <- df$estimated_possessions

  # Haslametrics-style per-100 trip rates (single-game, not opponent-adjusted).
  df$eff <- df$points_per_estimated_possession * 100
  df$ftar <- .espn_safe_div(df$free_throws_attempted * 100, df$estimated_possessions)
  df$fgar <- .espn_safe_div(df$field_goals_attempted * 100, df$estimated_possessions)
  df$threepar <- .espn_safe_div(df$three_point_field_goals_attempted * 100, df$estimated_possessions)
  df$pct_3pa <- .espn_safe_div(df$three_point_field_goals_attempted, df$field_goals_attempted)

  if (!is.null(minutes_col) && minutes_col %in% names(df)) {
    minutes_num <- .espn_minutes_to_numeric(df[[minutes_col]])
    df$minutes_numeric <- minutes_num
    df$points_per_minute <- .espn_safe_div(df$points, minutes_num)
  }

  df
}

# Remove repeated raw box-score columns once parsed equivalents exist.
.espn_drop_repeated_box_cols <- function(df) {
  drop_cols <- c(
    "fieldGoalsMade-fieldGoalsAttempted",
    "threePointFieldGoalsMade-threePointFieldGoalsAttempted",
    "freeThrowsMade-freeThrowsAttempted",
    "fieldGoalPct",
    "threePointFieldGoalPct",
    "freeThrowPct"
  )
  keep <- setdiff(names(df), drop_cols)
  dplyr::select(df, dplyr::all_of(keep))
}

# Home/away team ids from header competitions competitors.
.espn_home_away_team_ids <- function(game_obj) {
  comps <- .get_in(game_obj, "header", "competitions")
  if (is.null(comps) || length(comps) == 0L) {
    return(list(home_team_id = NA_character_, away_team_id = NA_character_))
  }
  competitors <- comps[[1]]$competitors
  if (is.null(competitors) || length(competitors) == 0L) {
    return(list(home_team_id = NA_character_, away_team_id = NA_character_))
  }
  home_team_id <- NA_character_
  away_team_id <- NA_character_
  for (cmp in competitors) {
    ha <- .null_or(cmp$homeAway, NA_character_)
    tid <- .null_or(.get_in(cmp, "team", "id"), NA_character_)
    if (identical(ha, "home")) home_team_id <- as.character(tid)
    if (identical(ha, "away")) away_team_id <- as.character(tid)
  }
  list(home_team_id = home_team_id, away_team_id = away_team_id)
}

# Box score: team-level statistics (one row per team, stats in columns)
.espn_parse_box_team_stats <- function(game_obj) {
  bx <- game_obj$boxscore
  if (is.null(bx) || is.null(bx$teams)) {
    return(tibble::tibble())
  }
  out <- purrr::map_dfr(bx$teams, function(t) {
    team_id <- .null_or(t$team$id, NA_character_)
    home_away <- .null_or(t$homeAway, NA_character_)
    stats <- t$statistics
    if (is.null(stats) || length(stats) == 0L) {
      return(tibble::tibble(team_id = team_id, home_away = home_away))
    }
    stat_names <- vapply(stats, function(s) .null_or(s$name, NA_character_), character(1L))
    stat_values <- vapply(stats, function(s) .null_or(s$displayValue, NA_character_), character(1L))
    names(stat_values) <- stat_names
    d <- as.list(stat_values)
    d$team_id <- team_id
    d$home_away <- home_away
    tibble::as_tibble(d)
  })
  if (nrow(out) == 0L) return(out)
  out <- .espn_split_made_attempted(out, "fieldGoalsMade-fieldGoalsAttempted", "field_goals")
  out <- .espn_split_made_attempted(out, "threePointFieldGoalsMade-threePointFieldGoalsAttempted", "three_point_field_goals")
  out <- .espn_split_made_attempted(out, "freeThrowsMade-freeThrowsAttempted", "free_throws")
  if ("field_goals_made" %in% names(out)) out$field_goals_made <- .espn_as_numeric(out$field_goals_made)
  if ("field_goals_attempted" %in% names(out)) out$field_goals_attempted <- .espn_as_numeric(out$field_goals_attempted)
  if ("three_point_field_goals_made" %in% names(out)) out$three_point_field_goals_made <- .espn_as_numeric(out$three_point_field_goals_made)
  if ("three_point_field_goals_attempted" %in% names(out)) out$three_point_field_goals_attempted <- .espn_as_numeric(out$three_point_field_goals_attempted)
  if ("free_throws_made" %in% names(out)) out$free_throws_made <- .espn_as_numeric(out$free_throws_made)
  if ("free_throws_attempted" %in% names(out)) out$free_throws_attempted <- .espn_as_numeric(out$free_throws_attempted)
  out <- .espn_add_box_advanced(out)
  out <- .espn_drop_repeated_box_cols(out)
  other <- setdiff(names(out), c("team_id", "home_away"))
  out <- dplyr::select(out, dplyr::all_of(c("team_id", "home_away")), dplyr::all_of(other))
  out
}

# Box score: player-level (one row per player)
.espn_parse_box_players <- function(game_obj, add_plus_minus = TRUE) {
  bx <- game_obj$boxscore
  players_section <- bx$players
  if (is.null(players_section) || length(players_section) == 0L) {
    return(tibble::tibble())
  }
  out <- purrr::map_dfr(players_section, function(team_block) {
    team_id <- .null_or(team_block$team$id, NA_character_)
    stats_block <- team_block$statistics
    if (is.null(stats_block) || length(stats_block) == 0L) return(tibble::tibble())
    keys <- stats_block[[1]]$keys
    if (is.null(keys)) keys <- character(0L)
    athletes <- stats_block[[1]]$athletes
    if (is.null(athletes) || length(athletes) == 0L) return(tibble::tibble())
    purrr::map_dfr(athletes, function(a) {
      ath <- a$athlete
      row <- list(
        team_id = team_id,
        player_id = .null_or(ath$id, NA_character_),
        display_name = .null_or(ath$displayName, NA_character_),
        jersey = .null_or(ath$jersey, NA_character_),
        position = if (is.null(ath$position)) NA_character_ else .null_or(ath$position$abbreviation, NA_character_),
        starter = isTRUE(a$starter),
        did_not_play = isTRUE(a$didNotPlay)
      )
      st <- a$stats
      if (length(keys) > 0L && length(st) > 0L) {
        for (i in seq_along(keys)) {
          if (i <= length(st)) row[[keys[[i]]]] <- st[[i]]
        }
      }
      tibble::as_tibble(row)
    })
  })
  if (nrow(out) == 0L) return(out)
  out <- .espn_split_made_attempted(out, "fieldGoalsMade-fieldGoalsAttempted", "field_goals")
  out <- .espn_split_made_attempted(out, "threePointFieldGoalsMade-threePointFieldGoalsAttempted", "three_point_field_goals")
  out <- .espn_split_made_attempted(out, "freeThrowsMade-freeThrowsAttempted", "free_throws")
  if ("field_goals_made" %in% names(out)) out$field_goals_made <- .espn_as_numeric(out$field_goals_made)
  if ("field_goals_attempted" %in% names(out)) out$field_goals_attempted <- .espn_as_numeric(out$field_goals_attempted)
  if ("three_point_field_goals_made" %in% names(out)) out$three_point_field_goals_made <- .espn_as_numeric(out$three_point_field_goals_made)
  if ("three_point_field_goals_attempted" %in% names(out)) out$three_point_field_goals_attempted <- .espn_as_numeric(out$three_point_field_goals_attempted)
  if ("free_throws_made" %in% names(out)) out$free_throws_made <- .espn_as_numeric(out$free_throws_made)
  if ("free_throws_attempted" %in% names(out)) out$free_throws_attempted <- .espn_as_numeric(out$free_throws_attempted)
  if ("rebounds" %in% names(out)) out$rebounds <- .espn_as_numeric(out$rebounds)
  if ("assists" %in% names(out)) out$assists <- .espn_as_numeric(out$assists)
  if ("turnovers" %in% names(out)) out$turnovers <- .espn_as_numeric(out$turnovers)
  if ("offensiveRebounds" %in% names(out)) out$offensiveRebounds <- .espn_as_numeric(out$offensiveRebounds)
  out <- .espn_add_box_advanced(out, minutes_col = "minutes")
  out <- .espn_drop_repeated_box_cols(out)

  if (isTRUE(add_plus_minus)) {
    # Prefer ESPN-provided plusMinus if present; fallback to summed on/off stints.
    if ("plusMinus" %in% names(out)) {
      out$plus_minus <- .espn_as_numeric(out$plusMinus)
    }
    if (!("plus_minus" %in% names(out)) || all(is.na(out$plus_minus))) {
      oo <- .espn_parse_on_off(game_obj, players_df = out)
      if (nrow(oo) > 0L) {
        pm <- oo |>
          dplyr::group_by(team_id, player_id) |>
          dplyr::summarise(plus_minus = sum(plus_minus, na.rm = TRUE), .groups = "drop")
        out <- dplyr::left_join(out, pm, by = c("team_id", "player_id"))
      } else {
        out$plus_minus <- NA_real_
      }
    }
  }

  out
}

# Game info: venue, attendance, officials (one row). Check top-level then boxscore.
.espn_parse_game_info <- function(game_obj) {
  gi <- .null_or(game_obj$gameInfo, game_obj$boxscore$gameInfo)
  if (is.null(gi)) return(tibble::tibble())
  venue_name <- NA_character_
  venue_id <- NA_character_
  if (!is.null(gi$venue)) {
    venue_name <- .null_or(gi$venue$fullName, NA_character_)
    venue_id <- .null_or(gi$venue$id, NA_character_)
  }
  attendance <- NA_integer_
  if (is.numeric(gi$attendance)) attendance <- as.integer(gi$attendance)
  officials_text <- NA_character_
  if (!is.null(gi$officials) && length(gi$officials) > 0L) {
    off_names <- vapply(gi$officials, function(o) .null_or(o$displayName, .null_or(o$fullName, NA_character_)), character(1L))
    officials_text <- paste(off_names, collapse = "; ")
  }
  tibble::tibble(
    venue_id = venue_id,
    venue_full_name = venue_name,
    attendance = attendance,
    officials = officials_text
  )
}

# Leaders: category, team, athlete, value (long table)
.espn_parse_leaders <- function(game_obj) {
  leaders_block <- game_obj$leaders
  if (is.null(leaders_block) || length(leaders_block) == 0L) {
    return(tibble::tibble())
  }
  out <- purrr::map_dfr(leaders_block, function(team_lead) {
    team_id <- .null_or(team_lead$team$id, NA_character_)
    team_display <- .null_or(team_lead$team$displayName, NA_character_)
    lead_list <- team_lead$leaders
    if (is.null(lead_list) || length(lead_list) == 0L) return(tibble::tibble())
    purrr::map_dfr(lead_list, function(cat) {
      cat_name <- .null_or(cat$name, NA_character_)
      cat_display <- .null_or(cat$displayName, NA_character_)
      leaders <- cat$leaders
      if (is.null(leaders) || length(leaders) == 0L) return(tibble::tibble())
      purrr::map_dfr(leaders, function(l) {
        ath <- l$athlete
        tibble::tibble(
          team_id = team_id,
          team_display_name = team_display,
          category = cat_name,
          category_display = cat_display,
          player_id = .null_or(ath$id, NA_character_),
          player_display_name = .null_or(ath$displayName, NA_character_),
          display_value = .null_or(l$displayValue, NA_character_),
          main_stat_value = .null_or(l$mainStat$value, NA_character_),
          main_stat_label = .null_or(l$mainStat$label, NA_character_)
        )
      })
    })
  })
  out
}

# Win probability: one row per play
.espn_parse_winprobability <- function(game_obj) {
  wp <- game_obj$winprobability
  if (is.null(wp) || length(wp) == 0L) return(tibble::tibble())
  team_ids <- .espn_home_away_team_ids(game_obj)
  out <- purrr::map_dfr(wp, function(n) {
    tibble::tibble(
      home_team_id = team_ids$home_team_id,
      away_team_id = team_ids$away_team_id,
      home_win_percentage = as.numeric(.null_or(n$homeWinPercentage, NA_real_)),
      tie_percentage = as.numeric(.null_or(n$tiePercentage, NA_real_)),
      play_id = as.character(.null_or(n$playId, NA_character_))
    )
  })
  out
}

# Pick center: odds/spread (one row per provider)
.espn_parse_pickcenter <- function(game_obj) {
  pc <- game_obj$pickcenter
  if (is.null(pc) || length(pc) == 0L) return(tibble::tibble())
  out <- purrr::map_dfr(pc, function(p) {
    tibble::tibble(
      provider_id = .null_or(.get_in(p, "provider", "id"), NA_character_),
      provider_name = .null_or(.get_in(p, "provider", "name"), NA_character_),
      details = .null_or(p$details, NA_character_),
      over_under = .null_or(p$overUnder, NA_character_),
      spread = .null_or(p$spread, NA_character_),
      over_odds = .null_or(p$overOdds, NA_character_),
      under_odds = .null_or(p$underOdds, NA_character_),
      away_team_id = .null_or(.get_in(p, "awayTeamOdds", "teamId"), NA_character_),
      away_moneyline = .null_or(.get_in(p, "awayTeamOdds", "moneyLine"), NA_character_),
      away_spread_odds = .null_or(.get_in(p, "awayTeamOdds", "spreadOdds"), NA_character_),
      away_favorite = as.logical(.null_or(.get_in(p, "awayTeamOdds", "favorite"), NA)),
      away_favorite_open = as.logical(.null_or(.get_in(p, "awayTeamOdds", "favoriteAtOpen"), NA)),
      home_team_id = .null_or(.get_in(p, "homeTeamOdds", "teamId"), NA_character_),
      home_moneyline = .null_or(.get_in(p, "homeTeamOdds", "moneyLine"), NA_character_),
      home_spread_odds = .null_or(.get_in(p, "homeTeamOdds", "spreadOdds"), NA_character_),
      home_favorite = as.logical(.null_or(.get_in(p, "homeTeamOdds", "favorite"), NA)),
      home_favorite_open = as.logical(.null_or(.get_in(p, "homeTeamOdds", "favoriteAtOpen"), NA)),
      home_moneyline_close = .null_or(.get_in(p, "moneyline", "home", "close", "odds"), NA_character_),
      home_moneyline_open = .null_or(.get_in(p, "moneyline", "home", "open", "odds"), NA_character_),
      away_moneyline_close = .null_or(.get_in(p, "moneyline", "away", "close", "odds"), NA_character_),
      away_moneyline_open = .null_or(.get_in(p, "moneyline", "away", "open", "odds"), NA_character_),
      home_spread_close = .null_or(.get_in(p, "pointSpread", "home", "close", "line"), NA_character_),
      home_spread_open = .null_or(.get_in(p, "pointSpread", "home", "open", "line"), NA_character_),
      away_spread_close = .null_or(.get_in(p, "pointSpread", "away", "close", "line"), NA_character_),
      away_spread_open = .null_or(.get_in(p, "pointSpread", "away", "open", "line"), NA_character_),
      total_over_close = .null_or(.get_in(p, "total", "over", "close", "line"), NA_character_),
      total_over_open = .null_or(.get_in(p, "total", "over", "open", "line"), NA_character_),
      total_under_close = .null_or(.get_in(p, "total", "under", "close", "line"), NA_character_),
      total_under_open = .null_or(.get_in(p, "total", "under", "open", "line"), NA_character_)
    )
  })
  out
}

# Play-by-play: one row per play
.espn_parse_plays <- function(game_obj) {
  plays <- game_obj$plays
  if (is.null(plays) || length(plays) == 0L) return(tibble::tibble())
  out <- purrr::map_dfr(plays, function(n) {
    participant_ids <- if (!is.null(n$participants)) {
      vapply(n$participants, function(p) .null_or(p$athlete$id, NA_character_), character(1L))
    } else {
      character(0L)
    }
    participant_ids <- c(participant_ids, NA_character_, NA_character_)
    coord_x <- if (is.null(n$coordinate)) NA_real_ else as.numeric(.null_or(n$coordinate$x, NA_real_))
    coord_y <- if (is.null(n$coordinate)) NA_real_ else as.numeric(.null_or(n$coordinate$y, NA_real_))
    tibble::tibble(
      play_id = as.character(.null_or(n$id, NA_character_)),
      sequence_number = as.character(.null_or(n$sequenceNumber, NA_character_)),
      play_type_id = as.character(.null_or(n$type$id, NA_character_)),
      play_type = as.character(.null_or(n$type$text, NA_character_)),
      text = as.character(.null_or(n$text, NA_character_)),
      away_score = as.numeric(.null_or(n$awayScore, NA_real_)),
      home_score = as.numeric(.null_or(n$homeScore, NA_real_)),
      period = as.numeric(.null_or(n$period$number, NA_real_)),
      period_display = as.character(.null_or(n$period$displayValue, NA_character_)),
      clock = as.character(.null_or(n$clock$displayValue, NA_character_)),
      scoring_play = as.logical(.null_or(n$scoringPlay, NA)),
      score_value = as.numeric(.null_or(n$scoreValue, NA_real_)),
      team_id = as.character(.null_or(n$team$id, NA_character_)),
      wall_clock = as.character(.null_or(n$wallclock, NA_character_)),
      shooting_play = as.logical(.null_or(n$shootingPlay, NA)),
      x_coordinate = coord_x,
      y_coordinate = coord_y,
      shot_distance = dplyr::if_else(
        isTRUE(as.logical(.null_or(n$shootingPlay, NA))) & !is.na(coord_x) & !is.na(coord_y),
        sqrt((coord_x - 25)^2 + (coord_y^2)),
        NA_real_
      ),
      shotDistance = dplyr::if_else(
        isTRUE(as.logical(.null_or(n$shootingPlay, NA))) & !is.na(coord_x) & !is.na(coord_y),
        sqrt((coord_x - 25)^2 + (coord_y^2)),
        NA_real_
      ),
      points_attempted = as.numeric(.null_or(n$pointsAttempted, NA_real_)),
      short_description = as.character(.null_or(n$shortDescription, NA_character_)),
      participant1_id = participant_ids[1L],
      participant2_id = participant_ids[2L]
    )
  })
  out
}

# Clock string "M:SS" to seconds remaining in period (NCAA 20-min halves)
.espn_clock_to_seconds_remaining <- function(clock_str) {
  if (is.na(clock_str) || length(clock_str) != 1L) return(NA_real_)
  parts <- strsplit(as.character(clock_str), ":", fixed = TRUE)[[1L]]
  if (length(parts) != 2L) return(NA_real_)
  minutes <- as.numeric(parts[1L])
  seconds <- as.numeric(parts[2L])
  if (is.na(minutes) || is.na(seconds)) return(NA_real_)
  minutes * 60 + seconds
}

# Game seconds elapsed (NCAA: 2 halves of 20 min)
.espn_clock_to_game_seconds <- function(period, clock_str) {
  if (is.na(period)) return(NA_real_)
  rem <- .espn_clock_to_seconds_remaining(clock_str)
  if (is.na(rem)) return(NA_real_)
  period_length <- 20 * 60
  (period - 1L) * period_length + (period_length - rem)
}

# On/off rotation stints with plus_minus (derived from plays + box)
.espn_parse_on_off <- function(game_obj, players_df = NULL) {
  plays <- game_obj$plays
  bx <- game_obj$boxscore
  if (is.null(plays) || length(plays) == 0L || is.null(bx$teams)) return(tibble::tibble())
  pbp <- .espn_parse_plays(game_obj)
  if (nrow(pbp) == 0L) return(tibble::tibble())
  pbp$game_seconds <- purrr::map2_dbl(pbp$period, pbp$clock, .espn_clock_to_game_seconds)
  pbp <- pbp[!is.na(pbp$game_seconds), , drop = FALSE]
  seq_num <- suppressWarnings(as.numeric(pbp$sequence_number))
  pbp <- pbp[order(pbp$game_seconds, seq_num), , drop = FALSE]
  players <- players_df
  if (is.null(players)) players <- .espn_parse_box_players(game_obj, add_plus_minus = FALSE)
  starters <- players[players$starter == TRUE, c("team_id", "player_id"), drop = FALSE]
  if (nrow(starters) == 0L) return(tibble::tibble())
  starters <- dplyr::distinct(starters)
  starter_n <- dplyr::count(starters, team_id, name = "n")
  if (any(starter_n$n != 5L)) return(tibble::tibble())
  team_ids <- unique(starters$team_id)
  home_away <- purrr::map_dfr(bx$teams, function(t) {
    tibble::tibble(team_id = .null_or(t$team$id, NA_character_), home_away = .null_or(t$homeAway, NA_character_))
  })
  subs <- pbp[pbp$play_type_id == "584", , drop = FALSE]
  sub_groups <- subs |>
    dplyr::filter(!is.na(team_id) & nzchar(team_id)) |>
    dplyr::group_by(team_id, period, clock) |>
    dplyr::summarise(
      game_seconds = dplyr::first(game_seconds),
      n_players = dplyr::n(),
      players = list(participant1_id[!is.na(participant1_id) & nzchar(participant1_id)]),
      .groups = "drop"
    ) |>
    dplyr::arrange(game_seconds)
  on_floor <- split(starters$player_id, starters$team_id)
  on_floor <- lapply(on_floor, as.character)
  start_times <- setNames(rep(list(setNames(numeric(0), character(0))), length(team_ids)), team_ids)
  for (tid in team_ids) {
    pids <- on_floor[[tid]]
    start_times[[tid]] <- setNames(rep(0, length(pids)), pids)
  }
  stints <- list()
  game_end <- max(pbp$game_seconds, na.rm = TRUE)
  for (i in seq_len(nrow(sub_groups))) {
    sub <- sub_groups[i, ]
    team <- sub$team_id
    participants <- unlist(sub$players, use.names = FALSE)
    current <- on_floor[[team]]
    if (is.null(current)) next
    going_out <- participants[participants %in% current]
    coming_in <- participants[!participants %in% current]
    if (length(going_out) == length(coming_in) && length(going_out) > 0L && sub$n_players %% 2L == 0L) {
      for (pid in going_out) {
        stints[[length(stints) + 1L]] <- tibble::tibble(
          team_id = team,
          player_id = pid,
          start_time = start_times[[team]][pid],
          end_time = sub$game_seconds
        )
      }
      on_floor[[team]] <- setdiff(current, going_out)
      on_floor[[team]] <- c(on_floor[[team]], coming_in)
      for (pid in going_out) start_times[[team]] <- start_times[[team]][names(start_times[[team]]) != pid]
      for (pid in coming_in) start_times[[team]][pid] <- sub$game_seconds
    }
  }
  for (tid in team_ids) {
    for (pid in names(start_times[[tid]])) {
      stints[[length(stints) + 1L]] <- tibble::tibble(
        team_id = tid,
        player_id = pid,
        start_time = start_times[[tid]][pid],
        end_time = game_end
      )
    }
  }
  if (length(stints) == 0L) return(tibble::tibble())
  stints_df <- dplyr::bind_rows(stints)
  stints_df$minutes <- (stints_df$end_time - stints_df$start_time) / 60
  team_is_home <- setNames(home_away$home_away == "home", home_away$team_id)
  get_score_at <- function(t) {
    rows <- pbp[pbp$game_seconds <= t, , drop = FALSE]
    if (nrow(rows) == 0L) return(list(away = 0, home = 0))
    r <- rows[nrow(rows), , drop = FALSE]
    list(away = r$away_score[1L], home = r$home_score[1L])
  }
  plus_minus <- numeric(nrow(stints_df))
  for (r in seq_len(nrow(stints_df))) {
    s <- stints_df$start_time[r]
    e <- stints_df$end_time[r]
    tid <- stints_df$team_id[r]
    sc_start <- get_score_at(s)
    sc_end <- get_score_at(e)
    team_pts <- if (team_is_home[tid]) (sc_end$home - sc_start$home) else (sc_end$away - sc_start$away)
    opp_pts <- if (team_is_home[tid]) (sc_end$away - sc_start$away) else (sc_end$home - sc_start$home)
    plus_minus[r] <- team_pts - opp_pts
  }
  stints_df$plus_minus <- plus_minus
  stints_df
}

# Save all game dataframes to game_dir
.espn_save_game_dataframes <- function(game_obj, game_dir) {
  game_dir <- .espn_db_ensure_dir(game_dir)
  box_players_df <- .espn_parse_box_players(game_obj)
  on_off_df <- .espn_parse_on_off(game_obj, players_df = box_players_df)
  df_list <- list(
    box_team_stats = .espn_parse_box_team_stats(game_obj),
    box_players = box_players_df,
    game_info = .espn_parse_game_info(game_obj),
    leaders = .espn_parse_leaders(game_obj),
    winprobability = .espn_parse_winprobability(game_obj),
    pickcenter = .espn_parse_pickcenter(game_obj),
    plays = .espn_parse_plays(game_obj),
    on_off = on_off_df
  )
  for (nm in names(df_list)) {
    path <- file.path(game_dir, paste0(nm, ".rds"))
    saveRDS(df_list[[nm]], path)
  }
  invisible(df_list)
}
