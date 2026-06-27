library(konenMCBB)
library(usethis)

season_ids <- c(18703, 18403, 18221)

ncaa_game_ids <- scrape_ncaa_game_ids(
  season_ids = season_ids,
  sleep_sec = 1
)

# Save into package
usethis::use_data(
  ncaa_game_ids,
  overwrite = TRUE,
  compress = "xz"
)
