test_that("season summary download works", {
  expect_no_error(download_season_summary(year = 2024))
})
