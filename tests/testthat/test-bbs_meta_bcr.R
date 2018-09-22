context("bbs_meta_bcr")

test_that("bbs_meta_bcr works correctly", {
  bbs_dir <- paste0(system.file("testdata", package = "rBBS"), '/')
  bcr <- bbs_meta_bcr(bbs_dir = bbs_dir)
  
  expect_is(bcr, "data.frame")
  expect_is(bcr, "tbl_df")
  expect_is(bcr$bcr, "integer")
  expect_is(bcr$bcr_name, "character")
})
