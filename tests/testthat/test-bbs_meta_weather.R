context("bbs_meta_weather")

test_that("bbs_meta_weather works correctly", {
  bbs_dir <- paste0(system.file("testdata", package = "rBBS"), '/')
  weather <- bbs_meta_weather(bbs_dir = bbs_dir)

  expect_is(weather, "data.frame")
  expect_is(weather, "tbl_df")
  expect_is(weather$country_num, "integer")
  expect_is(weather$state_num, "integer")
  expect_is(weather$route, "integer")
  expect_is(weather$year, "integer")
  expect_is(weather$run_type, "integer")
})
