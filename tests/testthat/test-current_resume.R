test_that("current_resume returns a dataset", {
  skip_on_cran()

  res <- current_resume()

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  expect_gt(ncol(res), 0)
})


