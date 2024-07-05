test_that("statsapi one-day download works", {
  expect_no_error(download_statsapi(start_date = "2024-07-01", end_date = "2024-07-01"))
})
