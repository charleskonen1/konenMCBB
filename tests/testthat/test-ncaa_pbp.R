test_that("ncaa_pbp validates game_id argument", {
  expect_error(ncaa_pbp(), "`game_id` must be a single value.", fixed = TRUE)
  expect_error(ncaa_pbp(c("1", "2")), "`game_id` must be a single value.", fixed = TRUE)
})


