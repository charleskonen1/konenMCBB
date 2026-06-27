test_that("teamRoster validates arguments", {
  expect_error(teamRoster(123, 2024), "`team` must be a single character string.", fixed = TRUE)
  expect_error(teamRoster(c("Duke", "UNC"), 2024), "`team` must be a single character string.", fixed = TRUE)
  expect_error(teamRoster("Duke", "2024"), "`season` must be a single numeric year.", fixed = TRUE)
  expect_error(teamRoster("Duke", c(2024, 2025)), "`season` must be a single numeric year.", fixed = TRUE)
})


