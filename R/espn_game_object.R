# Internal: game object is the parsed summary list. Save/load as game.rds for fast R use.

.espn_save_game_object <- function(game_obj, game_dir) {
  game_dir <- .espn_db_ensure_dir(game_dir)
  path <- file.path(game_dir, "game.rds")
  saveRDS(game_obj, path)
  invisible(path)
}

.espn_load_game_object <- function(game_dir) {
  path <- file.path(game_dir, "game.rds")
  if (!file.exists(path)) stop("Game object not found: ", path)
  readRDS(path)
}
