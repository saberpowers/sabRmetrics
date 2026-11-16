test_that("statsapi one-day download works", {
  expect_no_error(download_statsapi(start_date = "2024-07-01", end_date = "2024-07-01"))
})

# The expected count is the sum of `gameData$absChallenges` across the 15 game feeds for this date.
# It includes 8 challenges on strike three or ball four, which statsapi attaches to the event.
test_that("March 30, 2026, has 52 challenges", {
  march_30_2026 <- download_statsapi(start_date = '2026-03-30', end_date = '2026-03-30')
  n_challenges <- march_30_2026$pitch |>
    dplyr::filter(abs_challenged) |>
    nrow()
  expect_equal(n_challenges, 52)
})
