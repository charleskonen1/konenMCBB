test_that("timeMachine_ratings validates input date format and range", {
  expect_error(timeMachine_ratings(), "`date` must be a single value in YYYYMMDD format.", fixed = TRUE)
  expect_error(timeMachine_ratings("2024-01-01"), "`date` must be in YYYYMMDD format.", fixed = TRUE)
  expect_error(timeMachine_ratings(2010010), "`date` must be in YYYYMMDD format.", fixed = TRUE)
})


