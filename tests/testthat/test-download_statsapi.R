test_that("statsapi one-day download works", {
  expect_no_error(download_statsapi(start_date = "2024-07-01", end_date = "2024-07-01"))
})

test_that("March 30, 2026, has 44 challenges", {
  march_30_2026 <- download_statsapi(start_date = '2026-03-30', end_date = '2026-03-30')
  n_challenges <- march_30_2026$pitch |>
    dplyr::filter(abs_challenged) |>
    nrow()
  expect_equal(n_challenges, 44)
})
