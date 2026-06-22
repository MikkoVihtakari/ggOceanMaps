# Tiny synthetic bathymetry helper for tests:
#   left half negative (ocean), right half positive (land).
make_test_bathy <- function() {
  m <- matrix(c(rep(-100, 10 * 20), rep(50, 10 * 20)), nrow = 20, ncol = 20)
  s <- stars::st_as_stars(m)
  sf::st_set_crs(s, 4326)
}

test_that("vector_land returns an sf polygon layer from a depths=NULL bathy", {
  s <- make_test_bathy()
  bathy <- raster_bathymetry(s, depths = NULL, verbose = FALSE)

  land <- vector_land(bathy)

  expect_s3_class(land, "sf")
  expect_true("land" %in% names(land))
  expect_equal(unique(land$land), TRUE)
  expect_gt(nrow(land), 0)
  expect_equal(sf::st_crs(land), sf::st_crs(s))
})

test_that("vector_land works when raster_bathymetry was called with estimate.land = TRUE", {
  s <- make_test_bathy()
  bathy <- raster_bathymetry(
    s, depths = c(0, 50, 100), estimate.land = TRUE, verbose = FALSE
  )

  land <- vector_land(bathy)

  expect_s3_class(land, "sf")
  expect_gt(nrow(land), 0)
})

test_that("vector_land errors when bathy has no land", {
  m <- matrix(-100, nrow = 10, ncol = 10)
  s <- stars::st_as_stars(m)
  s <- sf::st_set_crs(s, 4326)
  bathy <- raster_bathymetry(s, depths = NULL, verbose = FALSE)

  expect_error(vector_land(bathy), "No land cells")
})

test_that("vector_land rejects non-bathyRaster input", {
  expect_error(vector_land(list(raster = 1)), "raster_bathymetry function")
})

test_that("vector_land rejects invalid drop.crumbs", {
  s <- make_test_bathy()
  bathy <- raster_bathymetry(s, depths = NULL, verbose = FALSE)

  expect_error(vector_land(bathy, drop.crumbs = c(1, 2)), "single value")
})

test_that("vector_land output can be passed to basemap shapefiles arg", {
  s <- make_test_bathy()
  bathy <- raster_bathymetry(s, depths = NULL, verbose = FALSE)
  land <- vector_land(bathy)

  # basemap should accept it without error
  expect_s3_class(
    basemap(shapefiles = list(land = land, glacier = NULL, bathy = NULL)),
    "gg"
  )
})
