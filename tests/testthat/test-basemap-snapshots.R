# Visual regression tests for basemap() — vdiffr snapshots.
# Run locally only (skip_on_cran + skip_on_ci) because SVG output
# is sensitive to sf/ggplot2/locale drift across platforms.
#
# Source corpus:
# 'ggOceanMaps development/tests/ggOceanMaps-function-tests.Rmd'

# Polar -------------------------------------------------------------------

test_that("polar: Arctic 60N", {
  skip_unless_visual()
  vdiffr::expect_doppelganger("polar-arctic-60", basemap(60))
})

test_that("polar: Antarctic -60", {
  skip_unless_visual()
  vdiffr::expect_doppelganger("polar-antarctic-60", basemap(limits = -60))
})

test_that("polar: Arctic rotated", {
  skip_unless_visual()
  vdiffr::expect_doppelganger(
    "polar-arctic-60-rotate",
    basemap(60, rotate = TRUE)
  )
})

# Square -----------------------------------------------------------------

test_that("square: non-DD with rotate + intervals", {
  skip_unless_visual()
  vdiffr::expect_doppelganger(
    "square-non-dd-rotate-intervals",
    basemap(c(40, 80, 60, 80), rotate = TRUE,
            lon.interval = 10, lat.interval = 2)
  )
})

test_that("square: Europe DD", {
  skip_unless_visual()
  vdiffr::expect_doppelganger(
    "square-europe-dd",
    basemap(limits = c(-20, 30, 40, 59))
  )
})

test_that("square: Europe DD with expand.factor", {
  skip_unless_visual()
  vdiffr::expect_doppelganger(
    "square-europe-expand",
    basemap(limits = c(-20, 30, 40, 59), expand.factor = 1.5)
  )
})

# Antimeridian -----------------------------------------------------------

test_that("antimeridian: limits rotated", {
  skip_unless_visual()
  vdiffr::expect_doppelganger(
    "antimeridian-160-neg160-rotate",
    basemap(limits = c(160, -160, 60, 80), rotate = TRUE)
  )
})

test_that("antimeridian: low-lat with bathymetry rotated", {
  skip_unless_visual()
  vdiffr::expect_doppelganger(
    "antimeridian-low-lat-bathy-rotate",
    basemap(limits = c(160, -160, 0, 30),
            rotate = TRUE, bathymetry = TRUE)
  )
})

test_that("antimeridian: issue #44 Atlantic", {
  skip_unless_visual()
  vdiffr::expect_doppelganger(
    "issue-44-atlantic-rotate",
    basemap(limits = c(40, -70, -37, 40), rotate = TRUE)
  )
})

# Projected --------------------------------------------------------------

test_that("projected: Arctic shapefiles", {
  skip_unless_visual()
  skip_if_no_largedata()
  vdiffr::expect_doppelganger(
    "projected-arctic",
    basemap(limits = c(2.5e4, -2.5e6, 2e6, -2.5e5), shapefiles = "Arctic")
  )
})

# crs argument -----------------------------------------------------------

test_that("crs: EPSG 32631 over North Sea", {
  skip_unless_visual()
  vdiffr::expect_doppelganger(
    "crs-32631-north-sea",
    basemap(limits = c(0, 15, 55, 65), crs = 32631)
  )
})

# data argument ----------------------------------------------------------

test_that("data: antimeridian rectangle rotated", {
  skip_unless_visual()
  vdiffr::expect_doppelganger(
    "data-antimeridian-rect-rotate",
    basemap(data.frame(lon = c(-160, 160, 160, -160),
                       lat = c(80, 80, 60, 60)), rotate = TRUE)
  )
})

# Bathymetry styles ------------------------------------------------------

test_that("bathy.style: rbb default raster binned blues", {
  skip_unless_visual()
  vdiffr::expect_doppelganger(
    "bathy-rbb",
    basemap(c(11, 16, 67.3, 68.6), bathy.style = "rbb")
  )
})

test_that("bathy.style: rcb continuous blues (needs largedata)", {
  skip_unless_visual()
  skip_if_no_largedata()
  vdiffr::expect_doppelganger(
    "bathy-rcb",
    basemap(c(11, 16, 67.3, 68.6), bathy.style = "rcb")
  )
})

test_that("bathy.style: pb poly blues (needs largedata)", {
  skip_unless_visual()
  skip_if_no_largedata()
  vdiffr::expect_doppelganger(
    "bathy-pb",
    basemap(c(11, 16, 67.3, 68.6), bathy.style = "pb")
  )
})

test_that("bathy.style: cb contour blues (needs largedata)", {
  skip_unless_visual()
  skip_if_no_largedata()
  vdiffr::expect_doppelganger(
    "bathy-cb",
    basemap(c(11, 16, 67.3, 68.6), bathy.style = "cb")
  )
})
