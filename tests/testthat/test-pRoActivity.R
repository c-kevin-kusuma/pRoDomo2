test_that("activity_ms_string never uses scientific notation", {
  expect_equal(activity_ms_string(1793232000000), "1793232000000")
  expect_equal(activity_ms_string(1793318400000), "1793318400000")
  expect_equal(activity_ms_string(1793232001234), "1793232001234")
  # a midnight UTC date that as.character() would turn into scientific notation
  ms <- floor(as.numeric(as.POSIXct(as.Date("2026-10-29"), tz = "UTC")) * 1000)
  expect_match(as.character(ms), "e\\+")
  expect_false(grepl("e", activity_ms_string(ms)))
})
