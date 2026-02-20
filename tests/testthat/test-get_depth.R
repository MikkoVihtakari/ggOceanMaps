test_that("get_depth works with data frame", {
  dt <- data.frame(lon = seq(-20, 80, length.out = 41), lat = 50:90)
  # bathy.style defaults to "raster_continuous" which requires download.
  # We use "raster_binned_blues" (rbb) which is shipped with the package.
  res <- get_depth(dt, bathy.style = "rbb", verbose = FALSE)
  expect_true(is.data.frame(res))
  expect_true("depth" %in% names(res))
})

test_that("get_depth returns vector when bind = FALSE", {
  dt <- data.frame(lon = c(0, 10), lat = c(60, 70))
  res <- get_depth(dt, bathy.style = "rbb", bind = FALSE, verbose = FALSE)
  expect_true(is.vector(res))
  expect_length(res, 2)
})
