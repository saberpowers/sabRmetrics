test_that("40-man roster download works", {
  expect_no_error(download_40man_roster(date = "2026-04-15"))
})
