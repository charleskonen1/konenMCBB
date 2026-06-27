test_that("teamList returns a non-empty data.frame with expected columns", {
  skip_on_cran()

  res <- teamList()

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 0)
  expect_named(res, c(
    "Rk","School","City, State",
    "From","To","Yrs","G","W","L",
    "W-L%","SRS","SOS","AP",
    "CREG","CRTN","NCAA","FF","NC"
  ))
})


