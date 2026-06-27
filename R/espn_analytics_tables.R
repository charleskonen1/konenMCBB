.espn_list_game_dirs <- function(base_path, season) {
  season <- as.character(season)
  season_dir <- .espn_db_season_dir(base_path, season)
  if (!dir.exists(season_dir)) {
    return(character(0L))
  }
  day_dirs <- list.dirs(season_dir, full.names = TRUE, recursive = FALSE)
  if (length(day_dirs) == 0L) {
    return(character(0L))
  }
  game_dirs <- unlist(
    lapply(day_dirs, function(d) {
      list.dirs(d, full.names = TRUE, recursive = FALSE)
    }),
    use.names = FALSE
  )
  game_dirs[dir.exists(game_dirs)]
}

espn_team_games <- function(season, base_path = .espn_db_base()) {
  season <- as.character(season)
  base_path <- base_path %||% .espn_db_base()

  game_dirs <- .espn_list_game_dirs(base_path, season)
  if (length(game_dirs) == 0L) {
    return(tibble::tibble())
  }

  out <- purrr::map_dfr(game_dirs, function(gdir) {
    parts <- strsplit(normalizePath(gdir, mustWork = FALSE), .Platform$file.sep)[[1L]]
    n <- length(parts)
    if (n < 3L) {
      return(NULL)
    }

    game_id <- parts[n]
    date_chr <- parts[n - 1L]
    season_chr <- parts[n - 2L]

    box_path <- file.path(gdir, "box_team_stats.rds")
    if (!file.exists(box_path)) {
      return(NULL)
    }

    df <- readRDS(box_path)
    if (is.null(df) || nrow(df) == 0L) {
      return(NULL)
    }

    df <- tibble::as_tibble(df)

    # Normalise known numeric stats that sometimes arrive as character
    # in older or mixed ESPN outputs, to avoid type conflicts when
    # binding rows across many games.
    num_cols <- intersect(
      c(
        "points",
        "field_goals_made", "field_goals_attempted",
        "three_point_field_goals_made", "three_point_field_goals_attempted",
        "free_throws_made", "free_throws_attempted",
        "rebounds", "offensiveRebounds", "offensive_rebounds",
        "defensiveRebounds", "defensive_rebounds",
        "assists", "turnovers"
      ),
      names(df)
    )
    for (nm in num_cols) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    }

    df$season <- season_chr
    df$game_date <- as.Date(date_chr)
    df$game_id <- game_id

    if ("home_away" %in% names(df)) {
      df$is_home <- df$home_away == "home"
      df$is_away <- df$home_away == "away"
      df$is_neutral <- !(df$is_home | df$is_away)
    }

    if ("team_id" %in% names(df) && nrow(df) == 2L) {
      df$opponent_team_id <- rev(df$team_id)
    } else {
      df$opponent_team_id <- NA_character_
    }

    if ("points" %in% names(df)) {
      pts <- df$points
      if (nrow(df) == 2L) {
        opp_pts <- rev(pts)
        df$points_allowed <- opp_pts
      } else {
        df$points_allowed <- NA_real_
      }
      df$margin <- df$points - df$points_allowed
      df$won <- !is.na(df$margin) & df$margin > 0
    } else {
      df$points_allowed <- NA_real_
      df$margin <- NA_real_
      df$won <- NA
    }

    df
  })

  tibble::as_tibble(out)
}

espn_player_games <- function(season, base_path = .espn_db_base()) {
  season <- as.character(season)
  base_path <- base_path %||% .espn_db_base()

  game_dirs <- .espn_list_game_dirs(base_path, season)
  if (length(game_dirs) == 0L) {
    return(tibble::tibble())
  }

  out <- purrr::map_dfr(game_dirs, function(gdir) {
    parts <- strsplit(normalizePath(gdir, mustWork = FALSE), .Platform$file.sep)[[1L]]
    n <- length(parts)
    if (n < 3L) {
      return(NULL)
    }

    game_id <- parts[n]
    date_chr <- parts[n - 1L]
    season_chr <- parts[n - 2L]

    box_path <- file.path(gdir, "box_players.rds")
    if (!file.exists(box_path)) {
      return(NULL)
    }

    df <- readRDS(box_path)
    if (is.null(df) || nrow(df) == 0L) {
      return(NULL)
    }

    df <- tibble::as_tibble(df)

    # Normalise key numeric stats that can appear as character in some games
    num_cols <- intersect(
      c(
        "points",
        "field_goals_made", "field_goals_attempted",
        "three_point_field_goals_made", "three_point_field_goals_attempted",
        "free_throws_made", "free_throws_attempted",
        "rebounds", "offensiveRebounds", "offensive_rebounds",
        "defensiveRebounds", "defensive_rebounds",
        "assists", "turnovers",
        "minutes_numeric",
        "estimated_possessions",
        "points_per_estimated_possession",
        "plus_minus"
      ),
      names(df)
    )
    for (nm in num_cols) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    }
    df$season <- season_chr
    df$game_date <- as.Date(date_chr)
    df$game_id <- game_id

    df
  })

  tibble::as_tibble(out)
}

