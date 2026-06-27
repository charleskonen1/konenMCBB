test_that("injuryList returns a data.frame (when page is available)", {
  skip_on_cran()

  res <- injuryList()

  expect_s3_class(res, "data.frame")
})


