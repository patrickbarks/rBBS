context("bbs_route_unique")

test_that("bbs_route_unique works correctly", {
  bbs_dir <- system.file("testdata", package = "rBBS")
  
  bcr <- bbs_meta_bcr(bbs_dir = bbs_dir)
  routes <- bbs_meta_routes(bbs_dir = bbs_dir)
  
  route_unique <- bbs_route_unique(routes)
  widths <- vapply(route_unique, nchar, integer(1))
  
  expect_true(class(route_unique) == "integer")
  expect_true(all(widths == 8))
  
  expect_error(bbs_route_unique(bcr))
  expect_error(bbs_route_unique(list(country_num = 1:3,
                                     state_num = 1:3,
                                     route = 1:3)))
})
