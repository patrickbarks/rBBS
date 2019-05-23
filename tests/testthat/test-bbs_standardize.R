context("bbs_standardize")

test_that("bbs_standardize works correctly", {
  bbs_dir <- paste0(system.file("testdata", package = "rBBS"), "/")
  
  routes <- csv_unzip(paste(bbs_dir, "routes.zip", sep = "/"))
  routes_st <- bbs_standardize(routes)
  
  regions <- read_bbs_txt(paste(bbs_dir, "RegionCodes.txt", sep = "/"))
  regions_st <- bbs_standardize(regions)
  
  expect_equal(class(routes), class(routes_st))
  expect_equal(names(routes_st), tolower(names(routes_st)))
  
  expect_true(all(grepl("Labrador", regions_st$state_name) == FALSE))
})
