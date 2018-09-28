context("bbs_download")

test_that("bbs_download works correctly", {
  bbs_dir <- paste0(system.file("testdata", package = "rBBS"), '/')
  
  temp <- tempdir()
  temp_sub <- paste(sample(letters, 6), collapse = "")
  temp_path <- paste(temp, temp_sub, sep = "/")
  temp_states <- paste(temp_path, "States", sep = "/")
  
  if (!dir.exists(temp_states)) {
    dir.create(temp_states, recursive = TRUE)
  }
  
  bbs_download(dest = temp_path, meta = FALSE, states = 'Nunavut')
  nunavut <- csv_unzip(paste(temp_states, "Nunavut.zip", sep = "/"))
  
  expect_is(nunavut, "data.frame")
  expect_is(nunavut, "tbl_df")
  
  expect_error(bbs_download(bbs_dir = temp_sub, states = 'Nunavut', bcr = 10))
  
  unlink(temp_states)
  unlink(temp_path)
})
