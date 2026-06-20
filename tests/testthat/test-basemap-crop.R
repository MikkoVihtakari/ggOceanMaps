test_that("projected clipping retains the requested northern land extent", {
  x <- basemap_data(
    limits = c(-20, 30, 50, 70),
    bathymetry = FALSE,
    expand.factor = 1
  )

  land_bbox <- sf::st_bbox(sf::st_transform(x$shapefiles$land, 4326))
  expect_gte(unname(land_bbox["ymax"]), 70)
  expect_false(any(sf::st_is_empty(x$shapefiles$land)))
})

test_that("wide antimeridian clipping returns finite non-empty land", {
  x <- basemap_data(
    limits = c(120, -120, 60, 80),
    bathymetry = FALSE,
    expand.factor = 1
  )

  expect_gt(sum(!sf::st_is_empty(x$shapefiles$land)), 0)
  expect_true(all(is.finite(sf::st_bbox(x$shapefiles$land))))
  expect_equal(sf::st_crs(x$shapefiles$land), sf::st_crs(x$proj))
})

test_that("bathyRaster cropping preserves source resolution", {
  bbox <- sf::st_bbox(
    c(xmin = 2, xmax = 3, ymin = 54, ymax = 55),
    crs = sf::st_crs(4326)
  )
  raster <- stars::st_as_stars(bbox, nx = 960, ny = 960)
  raster[[1]][] <- 100
  bathy <- list(raster = raster, depth.invervals = c(100, 100))
  class(bathy) <- "bathyRaster"
  attr(bathy, "wcs") <- TRUE

  x <- basemap_data(
    limits = c(2, 3, 54, 55),
    shapefiles = list(land = dd_land, glacier = NULL, bathy = bathy),
    bathymetry = TRUE,
    bathy.type = "raster_continuous",
    expand.factor = 1
  )

  expect_equal(dim(x$shapefiles$bathy$raster), c(x = 960L, y = 960L))
})

test_that("rotated antimeridian land has no unwrapped (seam-crossing) rings", {
  # Regression: basemap(c(100, -120, -12, -57), rotate = TRUE) (and similar)
  # produced land with a polygon ring "unwrapped" across the whole map after
  # transformation to the lon_0-shifted geographic CRS, which made ggplot2 fail
  # to draw the land at all (issue #44).
  x <- basemap_data(
    limits = c(100, -120, -12, -57),
    rotate = TRUE,
    bathymetry = FALSE,
    expand.factor = 1
  )
  ln <- x$shapefiles$land
  expect_false(any(sf::st_is_empty(ln)))
  expect_true(all(sf::st_is_valid(ln)))

  co <- sf::st_coordinates(ln)
  grp <- interaction(as.data.frame(co[, -(1:2), drop = FALSE]), drop = TRUE)
  spans <- tapply(co[, 1], grp, function(z) diff(range(z)))
  expect_true(all(spans <= 180))
})

test_that("default bathyRaster keeps source resolution (not the coarse stars default)", {
  # Regression: the warp target used stars::st_as_stars(bbox)'s ~255-cell
  # default, heavily downsampling the shipped dd_rbathy (7200x3600).
  x <- basemap_data(
    limits = c(-20, 30, 50, 70),
    bathymetry = TRUE,
    bathy.type = "raster_binned",
    expand.factor = 1
  )
  expect_gt(min(dim(x$shapefiles$bathy$raster)), 500)
})
