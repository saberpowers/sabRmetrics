test_that("schedule download works", {
  expect_no_error(download_schedule(start_date = "2024-01-01", end_date = "2024-12-31"))
})
