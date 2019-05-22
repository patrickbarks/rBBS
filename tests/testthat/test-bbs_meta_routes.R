context("bbs_meta_routes")

test_that("bbs_meta_routes works correctly", {
  #  skip_on_cran()
  bbs_dir <- system.file("testdata", package = "rBBS")
  routes <- bbs_meta_routes(bbs_dir = bbs_dir)
  
  expect_is(routes, "data.frame")
  expect_is(routes, "tbl_df")
  expect_is(routes$country_num, "integer")
  expect_is(routes$state_num, "integer")
  expect_is(routes$route, "integer")
  expect_is(routes$stratum, "integer")
  expect_is(routes$bcr, "integer")
  expect_is(routes$latitude, "numeric")
  expect_is(routes$active, "integer")
})
