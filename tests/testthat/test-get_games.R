test_that("get_games returns a dataset", {
  skip_on_cran()

  res <- get_games()

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  expect_gt(ncol(res), 0)
})

test_that("get_games validates quad argument", {
  expect_error(get_games(quad = 5), "`quad` must be 1, 2, 3, 4, or 'All'.", fixed = TRUE)
  expect_error(get_games(quad = "A"), "`quad` must be 1, 2, 3, 4, or 'All'.", fixed = TRUE)
})

