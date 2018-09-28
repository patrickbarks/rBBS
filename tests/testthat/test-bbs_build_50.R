context("bbs_build_50")

test_that("bbs_build_50 works correctly", {
  bbs_dir <- paste0(system.file("testdata", package = "rBBS"), '/')
  
  year_sub <- 2000:2015
  
  dat_50 <- bbs_build_50(bbs_dir, states = 'Nunavut')
  dat_50s <- bbs_build_50(bbs_dir, states = 'Nunavut', years = year_sub)
  dat_50z <- bbs_build_50(bbs_dir, zeros = TRUE, states = 'Nunavut')
  
  col_classes_50 <- vapply(dat_50, class, '', USE.NAMES = FALSE)
  col_classes_50z <- vapply(dat_50z, class, '', USE.NAMES = FALSE)
  
  col_names <- c('country_num', 'state_num', 'route', 'rpid', 'year', 'aou',
                 paste0('stop_', 1:50))
  
  expect_is(dat_50, "data.frame")
  expect_is(dat_50, "tbl_df")
  
  expect_is(dat_50z, "data.frame")
  expect_is(dat_50z, "tbl_df")
  
  expect_true(all(col_classes_50 == 'integer'))
  expect_true(all(col_classes_50z == 'integer'))
  
  expect_true(nrow(dat_50) > nrow(dat_50s))
  expect_true(nrow(dat_50z) > nrow(dat_50))
  
  expect_true(all(col_names %in% names(dat_50)))
  expect_true(all(col_names %in% names(dat_50z)))
  
  expect_true(all(dat_50s$year %in% year_sub))
  
  expect_error(bbs_build_50(bbs_dir = bbs_dir, states = 'Nunavut', bcr = 50))
})
