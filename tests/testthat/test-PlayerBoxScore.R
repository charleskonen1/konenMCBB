test_that("PlayerBoxScore validates arguments", {
  expect_error(PlayerBoxScore(123, 2024, "Purdue"), "`name` must be a single character string.", fixed = TRUE)
  expect_error(PlayerBoxScore(c("Zach Edey", "Other"), 2024, "Purdue"), "`name` must be a single character string.", fixed = TRUE)

  expect_error(PlayerBoxScore("Zach Edey", "2024", "Purdue"), "`season` must be a single numeric year.", fixed = TRUE)
  expect_error(PlayerBoxScore("Zach Edey", c(2024, 2025), "Purdue"), "`season` must be a single numeric year.", fixed = TRUE)

  expect_error(PlayerBoxScore("Zach Edey", 2024, 1), "`team` must be a single character string.", fixed = TRUE)
  expect_error(PlayerBoxScore("Zach Edey", 2024, c("Purdue", "Other")), "`team` must be a single character string.", fixed = TRUE)
})


