context("bbs_download")

test_that("bbs_download works correctly", {
  
  temp <- tempdir()
  
  temp_10 <- paste(temp, "States", sep = "/")
  temp_50 <- paste(temp, "50-StopData/1997ToPresent_SurveyWide", sep = "/")
  
  bbs_download(dest = temp, meta = FALSE, fifty_stop = TRUE,
               states = 'Nunavut')

  expect_message(
    bbs_download(dest = temp, meta = FALSE, states = 'Nunavut'),
    "The following files already exist", all = FALSE
  )

  expect_true("Nunavut.zip" %in% list.files(temp_10))
  expect_true("Fifty7.zip" %in% list.files(temp_50))

  expect_error(bbs_download(bbs_dir = temp_sub, countries = 'denmark'))
  expect_error(bbs_download(bbs_dir = temp_sub, states = 'denmark'))

  unlink(temp)
})
