test_that("ncaa_player_box validates game_id argument", {
  expect_error(ncaa_player_box(), "`game_id` must be a single value.", fixed = TRUE)
  expect_error(ncaa_player_box(c("1", "2")), "`game_id` must be a single value.", fixed = TRUE)
})


