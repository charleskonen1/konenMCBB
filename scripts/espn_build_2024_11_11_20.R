## Build ESPN game data for the 2024-25 season, Nov 11–20 (inclusive).
##
## Run this from the project root with:
##   source("scripts/espn_build_2024_11_11_20.R")
##
## This prefers the *development* version of konenMCBB in the current
## project (via devtools::load_all(".")) so that internal helpers like
## .espn_process_day are available, even if the installed package is older.

if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(".")
} else {
  if (!requireNamespace("konenMCBB", quietly = TRUE)) {
    stop("Package 'konenMCBB' must be installed and loadable, or devtools must be available.")
  }
  library(konenMCBB)
}

options(konenMCBB.espn_db_path = "data/espn_db")

dates <- seq(as.Date("2024-11-11"), as.Date("2024-11-20"), by = "1 day")

for (d in dates) {
  message("Processing ", d)
  tryCatch(
    {
      .espn_process_day(
        date      = d,
        season    = "2024-25",
        base_path = getOption("konenMCBB.espn_db_path"),
        sleep_sec = 0.5,
        espn_group = "50"
      )
    },
    error = function(e) {
      message("Failed on ", d, ": ", conditionMessage(e))
    }
  )
}

message("Done building ESPN data for 2024-11-11 through 2024-11-20.")

