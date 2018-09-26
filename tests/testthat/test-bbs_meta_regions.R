context("bbs_meta_regions")

test_that("bbs_meta_regions works correctly", {
  bbs_dir <- paste0(system.file("testdata", package = "rBBS"), '/')
  regions <- bbs_meta_regions(bbs_dir = bbs_dir, zip_files = TRUE)
  
  zip10 <- regions$ten_stop_file[!is.na(regions$ten_stop_file)]
  zip50 <- regions$fifty_stop_file[!is.na(regions$fifty_stop_file)]

  expect_is(regions, "data.frame")
  expect_is(regions, "tbl_df")
  expect_is(regions$country_num, "integer")
  expect_is(regions$country_name, "character")
  expect_is(regions$state_num, "integer")
  expect_is(regions$state_name, "character")
  expect_is(regions$ten_stop_file, "character")
  expect_is(regions$fifty_stop_file, "character")
  expect_true(all(grepl("\\.zip", zip10)))
  expect_true(all(grepl("\\.zip", zip50)))
})
