test_that("teamBoxScore validates arguments", {
  expect_error(teamBoxScore(team = 1), "`team` must be a single character string.", fixed = TRUE)
  expect_error(teamBoxScore(team = c("Duke", "UNC")), "`team` must be a single character string.", fixed = TRUE)
  expect_error(teamBoxScore(season = "2024"), "`season` must be a single numeric year.", fixed = TRUE)
  expect_error(teamBoxScore(season = c(2024, 2025)), "`season` must be a single numeric year.", fixed = TRUE)
})


