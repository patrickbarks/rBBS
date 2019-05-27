context("bbs_build_10")

test_that("bbs_build_10 works correctly", {
  bbs_dir <- paste0(system.file("testdata", package = "rBBS"), '/')
  
  year_sub <- 2000:2015
  aou_sub <- c(70, 100, 370)
  strata_sub <- 99
  
  dat_10 <- bbs_build_10(bbs_dir, states = 'Nunavut')
  dat_10z <- bbs_build_10(bbs_dir, zeros = TRUE, states = 'Nunavut')
  
  dat_10s1 <- bbs_build_10(bbs_dir, states = 'Nunavut', years = year_sub)
  dat_10s2 <- bbs_build_10(bbs_dir, states = 'Nunavut', aou = aou_sub)
  
  expect_warning(
    bbs_build_10(bbs_dir, countries = "Canada")
  )
  expect_warning(
    bbs_build_10(bbs_dir, bcr = 3)
  )
  expect_warning(
    dat_10s3 <- bbs_build_10(bbs_dir, strata = strata_sub)
  )
  expect_error(
    bbs_build_10(bbs_dir = bbs_dir, states = 'Nunavut', bcr = 10)
  )
  
  dat_10s3 <- merge(dat_10s3, bbs_meta_routes(bbs_dir))
  
  col_classes_10 <- vapply(dat_10, class, '', USE.NAMES = FALSE)
  col_classes_10z <- vapply(dat_10z, class, '', USE.NAMES = FALSE)
  
  col_names <- c('country_num', 'state_num', 'route', 'rpid', 'year', 'aou',
                 'count_10', 'count_20', 'count_30', 'count_40', 'count_50',
                 'stop_total', 'species_total')
  
  expect_is(dat_10, "data.frame")
  expect_is(dat_10, "tbl_df")
  
  expect_is(dat_10z, "data.frame")
  expect_is(dat_10z, "tbl_df")
  
  expect_true(all(col_classes_10 == 'integer'))
  expect_true(all(col_classes_10z == 'integer'))
  
  expect_true(nrow(dat_10) > nrow(dat_10s1))
  expect_true(nrow(dat_10z) > nrow(dat_10))
  
  expect_true(all(col_names %in% names(dat_10)))
  expect_true(all(col_names %in% names(dat_10z)))
  
  expect_true(all(dat_10s1$year %in% year_sub))
  expect_true(all(dat_10s2$aou %in% aou_sub))
  expect_true(all(dat_10s3$strata %in% strata_sub))
})
