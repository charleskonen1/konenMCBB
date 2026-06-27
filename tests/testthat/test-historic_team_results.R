test_that("historic_team_results validates year argument", {
  expect_error(historic_team_results("2020"), "`year` must be numeric and >= 2008.", fixed = TRUE)
  expect_error(historic_team_results(2000), "`year` must be numeric and >= 2008.", fixed = TRUE)
})

test_that("historic_team_results returns a data.frame for a recent year", {
  skip_on_cran()

  res <- historic_team_results(2024)

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
})


