test_that("get_super_sked validates year argument", {
  expect_error(get_super_sked("2020"), "`year` must be numeric and >= 2008.", fixed = TRUE)
  expect_error(get_super_sked(2000), "`year` must be numeric and >= 2008.", fixed = TRUE)
})

test_that("get_super_sked returns a data.frame for a recent year", {
  skip_on_cran()

  res <- get_super_sked(2024)

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
})


