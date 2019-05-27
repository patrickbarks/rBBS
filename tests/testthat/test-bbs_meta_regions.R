context("bbs_meta_regions")

test_that("bbs_meta_regions works correctly", {
  bbs_dir <- paste0(system.file("testdata", package = "rBBS"), '/')
  
  regions <- bbs_meta_regions(bbs_dir = bbs_dir)
  regions_zip <- bbs_meta_regions(bbs_dir = bbs_dir, zip_files = TRUE)
  
  zip10 <- regions_zip$ten_stop_file[!is.na(regions_zip$ten_stop_file)]
  zip50 <- regions_zip$fifty_stop_file[!is.na(regions_zip$fifty_stop_file)]

  expect_true(ncol(regions_zip) == ncol(regions) + 2)
  expect_true(all(c("data.frame", "tbl_df") %in% class(regions)))
  expect_is(regions$country_num, "integer")
  expect_is(regions$country_name, "character")
  expect_is(regions$state_num, "integer")
  expect_is(regions$state_name, "character")
  expect_is(regions_zip$ten_stop_file, "character")
  expect_is(regions_zip$fifty_stop_file, "character")
  expect_true(all(grepl("\\.zip", zip10)))
  expect_true(all(grepl("\\.zip", zip50)))
  
  # read from ftp
  regions2 <- bbs_meta_regions(zip_files = TRUE)
  expect_is(regions2, "data.frame")
  expect_is(regions2$ten_stop_file, "character")
  expect_is(regions2$fifty_stop_file, "character")
})
