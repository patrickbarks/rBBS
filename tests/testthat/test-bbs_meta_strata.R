context("bbs_meta_strata")

test_that("bbs_meta_strata works correctly", {
  bbs_dir <- paste0(system.file("testdata", package = "rBBS"), '/')
  strata <- bbs_meta_strata(bbs_dir = bbs_dir)
  
  expect_is(strata, "data.frame")
  expect_is(strata, "tbl_df")
  expect_is(strata$stratum, "integer")
  expect_is(strata$stratum_name, "character")
  expect_is(strata$stratum_area, "integer")
})
