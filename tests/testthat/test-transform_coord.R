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

test_that("transform_coord removes rows with either missing coordinate", {
  x <- data.frame(
    lon = c(1, NA, NA, 4),
    lat = c(2, 3, NA, NA),
    label = letters[1:4],
    row.names = paste0("row", 1:4)
  )

  out <- transform_coord(x, proj.out = 4326, na = "remove", bind = TRUE)

  expect_equal(out$label, "a")
  expect_equal(rownames(out), "row1")
  expect_equal(out$lon.proj, 1)
})

test_that("transform_coord removes all rows when every coordinate pair is incomplete", {
  x <- data.frame(lon = c(NA, 1), lat = c(2, NA), label = c("a", "b"))

  out <- transform_coord(x, na = "remove", bind = TRUE)

  expect_equal(nrow(out), 0)
  expect_true(all(c("lon.proj", "lat.proj", "label") %in% names(out)))
})

test_that("transform_coord leaves complete coordinates unchanged by NA removal", {
  x <- data.frame(lon = c(1, 4), lat = c(2, 5))

  out <- transform_coord(x, proj.out = 4326, na = "remove", bind = TRUE)

  expect_equal(nrow(out), nrow(x))
  expect_equal(out$lon.proj, x$lon)
  expect_equal(out$lat.proj, x$lat)
})
