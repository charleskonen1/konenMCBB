## Shiny app entrypoint (kept outside package root R/ folder)
##
## Run with:
##   shiny::runApp("apps/team_dashboard")
##
## This avoids Shiny's warning about loading R/ when the appDir is a package,
## and ensures the ESPN DB path points at the project-level data/espn_db.

root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
options(konenMCBB.espn_db_path = file.path(root, "data", "espn_db"))

source(file.path(root, "team_dashboard_app.R"), local = TRUE)

