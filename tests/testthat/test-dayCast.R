test_that("dayCast returns informative result", {
  skip_on_cran()

  res <- dayCast()

  if (is.character(res)) {
    expect_identical(res, "No games scheduled for today.")
  } else {
    expect_s3_class(res, "data.frame")
  }
})


