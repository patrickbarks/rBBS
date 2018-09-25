context("add_route_unique")

test_that("add_route_unique works correctly", {
  bbs_dir <- system.file("testdata", package = "rBBS")
  
  bcr <- bbs_meta_bcr(bbs_dir = bbs_dir)
  routes <- bbs_meta_routes(bbs_dir = bbs_dir)
  
  routes_add <- add_route_unique(routes)
  
  expect_length(routes_add, ncol(routes) + 1)
  expect_true("route_unique" %in% names(routes_add))
  expect_true(nrow(routes) == nrow(routes_add))
  expect_error(add_route_unique(bcr))
  expect_error(add_route_unique(routes_add))
  expect_error(add_route_unique(routes, col_name = 134))
})
