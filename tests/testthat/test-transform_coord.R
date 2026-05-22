test_that("transform_coord auto-detects polar projection for high latitudes", {
  x <- data.frame(lon = c(-150, 150), lat = c(60, 90))
  out <- transform_coord(x, bind = TRUE)
  expect_true(all(c("lon.proj", "lat.proj") %in% names(out)))
  # Polar stereographic projection produces non-DD coordinate values
  expect_false(identical(out$lon, out$lon.proj))
})

test_that("transform_coord skips transform when no projection needed", {
  x <- data.frame(lon = c(-150, 150), lat = c(20, 50))
  out <- transform_coord(x, bind = TRUE)
  # Non-polar latitudes stay in decimal degrees
  expect_equal(out$lon, out$lon.proj)
  expect_equal(out$lat, out$lat.proj)
})

test_that("transform_coord with explicit proj.out works", {
  x <- data.frame(lon = c(0, 15), lat = c(55, 65))
  out <- transform_coord(x, proj.out = 32631, bind = TRUE)
  expect_true(all(c("lon.proj", "lat.proj") %in% names(out)))
  expect_false(identical(out$lon, out$lon.proj))
})

test_that("transform_coord supports numeric vector input", {
  out <- transform_coord(lon = c(-150, 150), lat = c(60, 90))
  expect_true(all(c("lon", "lat") %in% names(out)))
  expect_equal(nrow(out), 2)
})
