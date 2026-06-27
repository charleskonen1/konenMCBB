# Internal path helpers for ESPN game database
# Structure: base_path / season / date / game_id / (raw.json, game.rds, *.rds)
#
# Default base is R's package user data dir (persists across reinstalls, app-specific).
# Override with: options(konenMCBB.espn_db_path = "/path/to/espn_db")
# e.g. project-relative: options(konenMCBB.espn_db_path = "data/espn_db")

.espn_db_base <- function() {
  opt <- getOption("konenMCBB.espn_db_path")
  if (is.character(opt) && length(opt) == 1L && nzchar(opt)) {
    return(opt)
  }
  file.path(tools::R_user_dir("konenMCBB", which = "data"), "espn_db")
}

.espn_db_season_dir <- function(base_path, season) {
  stopifnot(is.character(season), length(season) == 1L)
  file.path(base_path, as.character(season))
}

.espn_db_day_dir <- function(base_path, season, date) {
  d <- as.Date(date)
  file.path(.espn_db_season_dir(base_path, season), format(d, "%Y-%m-%d"))
}

.espn_db_game_dir <- function(base_path, season, date, game_id) {
  file.path(.espn_db_day_dir(base_path, season, date), as.character(game_id))
}

# Ensure directory exists; return path
.espn_db_ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}
