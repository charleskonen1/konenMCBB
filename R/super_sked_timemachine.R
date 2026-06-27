#' Super schedule joined with Time Machine ratings
#'
#' For a given date, retrieve the Bart Torvik super schedule and Time Machine
#' team ratings, and return a long data.frame where each row is a single
#' team-game. Each game appears twice (once from each team's perspective),
#' with team and opponent pre-game ratings plus the final score.
#'
#' @param date Date in YYYYMMDD format (must satisfy \code{timeMachine_ratings()}).
#'
#' @return A data.frame with one row per team-game, including:
#'   \itemize{
#'     \item \code{game_date}: game date as Date
#'     \item \code{muid}: Torvik game id
#'     \item \code{team}, \code{opponent}: team names
#'     \item \code{team_pts}, \code{opp_pts}: final score from each team's perspective
#'     \item \code{team_*}: selected Time Machine metrics for the team
#'     \item \code{opp_*}: the same metrics for the opponent
#'   }
#'
#' @examples
#' \dontrun{
#'   # 2024-11-15 slate with Time Machine ratings
#'   df <- super_sked_with_timemachine(20241115)
#' }
#'
#' @export
super_sked_with_timemachine <- function(date) {

  if (missing(date) || length(date) != 1L) {
    stop("`date` must be a single value in YYYYMMDD format.")
  }
  if (!grepl("^[0-9]{8}$", as.character(date))) {
    stop("`date` must be in YYYYMMDD format.")
  }

  date_int <- as.integer(date)
  target_date <- as.Date(as.character(date_int), format = "%Y%m%d")

  # Time Machine ratings for that date
  tm <- timeMachine_ratings(date_int)

  # Keep core metrics we are likely to want on the dashboard / models
  # Column names are derived in timeMachine_ratings(); we reference them by
  # their human-readable headers.
  tm_metrics <- tm
  # Normalise names to something safe; make.unique() already applied in helper.
  # We expect at least: "team", "adjoe", "adjde", "barthag", "adjt", "WAB".
  if (!"team" %in% names(tm_metrics)) {
    stop("Time Machine data does not contain a 'team' column; structure may have changed upstream.")
  }

  tm_sel <- tm_metrics |>
    dplyr::select(
      team,
      conf        = .data$conf,
      adjoe       = .data$adjoe,
      adjde       = .data$adjde,
      barthag     = .data$barthag,
      proj_W      = .data$`proj. W`,
      proj_L      = .data$`Proj. L`,
      fun         = .data$FUN,
      WAB         = .data$WAB
    )

  # Super schedule for the appropriate season year
  season_year <- as.integer(substr(as.character(date_int), 1L, 4L))
  sked <- get_super_sked(season_year)
  sked$Date <- as.Date(sked$Date, format = "%m/%d/%y")

  games <- sked |>
    dplyr::filter(.data$Date == target_date)

  if (nrow(games) == 0L) {
    # Return an empty but correctly-shaped data.frame
    return(
      data.frame(
        game_date = as.Date(character()),
        muid = character(),
        team = character(),
        opponent = character(),
        team_pts = integer(),
        opp_pts = integer(),
        team_conf = character(),
        team_adjoe = numeric(),
        team_adjde = numeric(),
        team_barthag = numeric(),
        team_proj_W = numeric(),
        team_proj_L = numeric(),
        team_fun = numeric(),
        team_WAB = numeric(),
        opp_conf = character(),
        opp_adjoe = numeric(),
        opp_adjde = numeric(),
        opp_barthag = numeric(),
        opp_proj_W = numeric(),
        opp_proj_L = numeric(),
        opp_fun = numeric(),
        opp_WAB = numeric(),
        stringsAsFactors = FALSE
      )
    )
  }

  # Long form: one row per team per game
  long <- games |>
    dplyr::transmute(
      game_date = .data$Date,
      muid      = .data$muid,
      team1     = .data$team1,
      team2     = .data$team2,
      t1pts     = as.integer(.data$t1pts),
      t2pts     = as.integer(.data$t2pts)
    ) |>
    tidyr::pivot_longer(
      cols = c("team1", "team2"),
      names_to = "side",
      values_to = "team"
    ) |>
    dplyr::mutate(
      opponent = dplyr::if_else(.data$side == "team1", .data$team2, .data$team1),
      team_pts = dplyr::if_else(.data$side == "team1", .data$t1pts, .data$t2pts),
      opp_pts  = dplyr::if_else(.data$side == "team1", .data$t2pts, .data$t1pts)
    ) |>
    dplyr::select(.data$game_date, .data$muid, .data$team, .data$opponent, .data$team_pts, .data$opp_pts)

  # Join team and opponent Time Machine ratings
  out <- long |>
    dplyr::left_join(
      tm_sel,
      by = c("team" = "team"),
      suffix = c("", "_tm")
    ) |>
    dplyr::rename_with(
      ~ paste0("team_", .x),
      c("conf", "adjoe", "adjde", "barthag", "proj_W", "proj_L", "fun", "WAB")
    ) |>
    dplyr::left_join(
      tm_sel,
      by = c("opponent" = "team"),
      suffix = c("", "_opp")
    ) |>
    dplyr::rename_with(
      ~ paste0("opp_", sub("_opp$", "", .x)),
      dplyr::ends_with("_opp")
    )

  out
}

