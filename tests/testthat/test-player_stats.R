test_that("player_stats validates year argument", {
  expect_error(player_stats("2020"), "`year` must be numeric and >= 2008.", fixed = TRUE)
  expect_error(player_stats(2000), "`year` must be numeric and >= 2008.", fixed = TRUE)
})

test_that("player_stats returns a tibble for a recent year", {
  skip_on_cran()

  res <- player_stats(2024)

  expect_s3_class(res, "tbl_df")
  expect_gt(nrow(res), 0)
})


