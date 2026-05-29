# Smoke tests for basemap(): assert no error + ggplot return class.
# Cases distilled from the regression corpus in
# 'ggOceanMaps development/tests/ggOceanMaps-function-tests.Rmd'.

# Polar -------------------------------------------------------------------

test_that("polar basemap (Arctic) builds", {
  expect_s3_class(basemap(60), "gg")
})

test_that("polar basemap with rotate builds", {
  expect_s3_class(basemap(60, rotate = TRUE), "gg")
})

test_that("polar basemap (Antarctic) builds", {
  expect_s3_class(basemap(limits = -60), "gg")
})

test_that("polar basemap with default bathymetry builds", {
  expect_s3_class(basemap(60, bathymetry = TRUE), "gg")
})

test_that("polar basemap with glaciers builds when shapefiles forced", {
  skip_if_no_largedata()
  expect_s3_class(basemap(60, glaciers = TRUE, shapefiles = "Arc"), "gg")
})

# Square (non-DD) --------------------------------------------------------

test_that("non-DD square basemap builds", {
  expect_s3_class(basemap(c(40, 80, 60, 80)), "gg")
})

test_that("non-DD square basemap with rotate + intervals builds", {
  expect_s3_class(
    basemap(c(40, 80, 60, 80), rotate = TRUE,
            lon.interval = 10, lat.interval = 2),
    "gg"
  )
})

test_that("global extent c(-180, 180, -90, 90) builds", {
  expect_s3_class(basemap(c(-180, 180, -90, 90)), "gg")
})

test_that("Europe DD square builds", {
  expect_s3_class(basemap(limits = c(-20, 30, 40, 59)), "gg")
})

test_that("Europe DD square with rotate builds", {
  expect_s3_class(basemap(limits = c(-20, 30, 40, 59), rotate = TRUE), "gg")
})

test_that("Europe DD with expand.factor builds", {
  expect_s3_class(
    basemap(limits = c(-20, 30, 40, 59), expand.factor = 1.5),
    "gg"
  )
})

test_that("North Pacific DD with rotate builds", {
  expect_s3_class(
    basemap(limits = c(-160, -80, 60, 85), rotate = TRUE),
    "gg"
  )
})

# Antimeridian ------------------------------------------------------------

test_that("antimeridian limits without rotate produce a plot (with message)", {
  expect_s3_class(basemap(limits = c(160, -160, 60, 80)), "gg")
})

test_that("antimeridian limits with rotate build", {
  expect_s3_class(basemap(limits = c(160, -160, 60, 80), rotate = TRUE), "gg")
})

test_that("antimeridian DD low-latitude with rotate + bathymetry builds", {
  expect_s3_class(
    basemap(limits = c(160, -160, 0, 30), rotate = TRUE, bathymetry = TRUE),
    "gg"
  )
})

test_that("issue #44 Atlantic crossing 0-meridian with rotate builds", {
  # https://github.com/MikkoVihtakari/ggOceanMaps/issues/44
  expect_s3_class(basemap(limits = c(40, -70, -37, 40), rotate = TRUE), "gg")
})

test_that("South Pacific wide rotated build", {
  expect_s3_class(
    basemap(c(100, -120, -12, -57), rotate = TRUE),
    "gg"
  )
})

# Projected limits -------------------------------------------------------

test_that("projected limits with explicit shapefiles build", {
  skip_if_no_largedata()
  expect_s3_class(
    basemap(limits = c(2.5e4, -2.5e6, 2e6, -2.5e5), shapefiles = "Arctic"),
    "gg"
  )
})

test_that("projected limits with expand.factor build", {
  skip_if_no_largedata()
  expect_s3_class(
    basemap(limits = c(2.5e4, -2.5e6, 2e6, -2.5e5),
            shapefiles = "Arctic", expand.factor = 1.3),
    "gg"
  )
})

# Premade shapefile shortcut ---------------------------------------------

test_that("premade shapefile shortcut builds", {
  skip_if_no_largedata()
  expect_s3_class(basemap("ArcticStereographic"), "gg")
})

# crs argument -----------------------------------------------------------

test_that("custom crs via EPSG code builds", {
  expect_s3_class(basemap(limits = c(0, 15, 55, 65), crs = 32631), "gg")
})

test_that("crs = 4326 fine-resolution builds", {
  expect_s3_class(
    basemap(c(10, 17, 66.8, 69.2),
            lon.interval = 1, lat.interval = 0.2, crs = 4326),
    "gg"
  )
})

# data argument ----------------------------------------------------------

test_that("data.frame data argument builds", {
  expect_s3_class(
    basemap(data.frame(lon = c(40, 80), lat = c(60, 80)), rotate = TRUE),
    "gg"
  )
})

test_that("data.frame antimeridian rectangle builds", {
  expect_s3_class(
    basemap(data.frame(lon = c(-160, 160, 160, -160),
                       lat = c(80, 80, 60, 60))),
    "gg"
  )
})

test_that("data.frame antimeridian rectangle with rotate builds", {
  expect_s3_class(
    basemap(data.frame(lon = c(-160, 160, 160, -160),
                       lat = c(80, 80, 60, 60)), rotate = TRUE),
    "gg"
  )
})

test_that("sf input builds", {
  pts <- sf::st_sfc(
    sf::st_multipoint(as.matrix(
      data.frame(lon = c(-150, 150), lat = c(60, 85))
    )),
    crs = 4326
  )
  expect_s3_class(basemap(pts), "gg")
})

test_that("sp input builds", {
  skip_if_not_installed("sp")
  pts <- sp::SpatialPoints(
    data.frame(lon = c(-150, 150), lat = c(60, 85)),
    proj4string = sp::CRS(SRS_string = "EPSG:4326")
  )
  expect_s3_class(basemap(pts), "gg")
})

# qmap -------------------------------------------------------------------

test_that("qmap with red dots builds", {
  expect_s3_class(
    qmap(data.frame(lon = c(40, 80), lat = c(60, 80)),
         color = I("red"), rotate = TRUE, lon.interval = 10),
    "gg"
  )
})

# Bathymetry styles (those that need ggOceanMapsLargeData skip-if-absent) -

test_that("default raster_binned_blues bathymetry builds at fine extent", {
  expect_s3_class(
    basemap(c(11, 16, 67.3, 68.6), bathymetry = TRUE),
    "gg"
  )
})

test_that("raster_continuous_blues needs largedata", {
  skip_if_no_largedata()
  expect_s3_class(
    basemap(c(11, 16, 67.3, 68.6), bathy.style = "rcb"),
    "gg"
  )
})

test_that("raster_user_blues needs userpath", {
  skip_if_no_userpath()
  expect_s3_class(
    basemap(c(11, 16, 67.3, 68.6), bathy.style = "rub"),
    "gg"
  )
})

test_that("poly_blues needs largedata", {
  skip_if_no_largedata()
  expect_s3_class(
    basemap(c(11, 16, 67.3, 68.6), bathy.style = "pb"),
    "gg"
  )
})

test_that("contour_blues needs largedata", {
  skip_if_no_largedata()
  expect_s3_class(
    basemap(c(11, 16, 67.3, 68.6), bathy.style = "cb"),
    "gg"
  )
})
