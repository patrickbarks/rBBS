context("bbs_meta_species")

test_that("bbs_meta_species works correctly", {
  bbs_dir <- paste0(system.file("testdata", package = "rBBS"), '/')
  species <- bbs_meta_species(bbs_dir = bbs_dir)
  
  expect_is(species, "data.frame")
  expect_is(species, "tbl_df")
  expect_is(species$seq, "integer")
  expect_is(species$aou, "integer")
  expect_is(species$english_common_name, "character")
  expect_is(species$order, "character")
  expect_is(species$family, "character")
  expect_is(species$genus, "character")
})
