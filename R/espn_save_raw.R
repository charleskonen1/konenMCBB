# Internal: save raw game JSON to game folder. Writes raw.json into game_dir.

.espn_save_raw <- function(game_obj, game_dir) {
  game_dir <- .espn_db_ensure_dir(game_dir)
  path <- file.path(game_dir, "raw.json")
  jsonlite::write_json(game_obj, path, auto_unbox = TRUE, null = "null", digits = NA)
  invisible(path)
}
