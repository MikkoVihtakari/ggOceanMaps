# WCS tests hit a live external service; gate on CRAN and offline runners.

skip_if_no_internet <- function() {
  testthat::skip_on_cran()
  testthat::skip_if_offline()
}

# Input validation (always runs — pure local logic) ----------------------

test_that("wcs_bathymetry rejects malformed limits", {
  expect_error(wcs_bathymetry(c(1, 2, 3)), "length 4")
  expect_error(wcs_bathymetry(c(2, 1, 54, 55)), "xmin.*less than.*xmax")
  expect_error(wcs_bathymetry(c(1, 2, 55, 54)), "ymin.*less than.*ymax")
})

test_that("wcs_bathymetry enforces max_area_deg2", {
  expect_error(
    wcs_bathymetry(c(0, 20, 50, 70), max_area_deg2 = 10),
    "exceeds max_area_deg2"
  )
})

test_that("wcs_bathymetry rejects unknown sources", {
  expect_error(wcs_bathymetry(c(2, 3, 54, 55), source = "nonsense"))
})

# Internal helper tests --------------------------------------------------

test_that("wcs_build_url produces a well-formed GetCoverage URL", {
  src <- wcs_source_info("emodnet")
  url <- wcs_build_url(src, c(xmin = 2, xmax = 3, ymin = 54, ymax = 55))
  expect_match(url, "ows\\.emodnet-bathymetry\\.eu/wcs")
  expect_match(url, "REQUEST=GetCoverage")
  expect_match(url, "CoverageId=emodnet__mean")
  expect_match(url, "SUBSET=Lat\\(54")
  expect_match(url, "SUBSET=Long\\(2")
})

test_that("wcs_cache_path is deterministic and bbox-keyed", {
  src <- wcs_source_info("emodnet")
  p1 <- wcs_cache_path(tempdir(), src, c(xmin = 2, xmax = 3, ymin = 54, ymax = 55))
  p2 <- wcs_cache_path(tempdir(), src, c(xmin = 2, xmax = 3, ymin = 54, ymax = 55))
  p3 <- wcs_cache_path(tempdir(), src, c(xmin = 2, xmax = 4, ymin = 54, ymax = 55))
  expect_identical(p1, p2)
  expect_false(identical(p1, p3))
  expect_match(p1, "\\.tif$")
})

# Live network tests -----------------------------------------------------

test_that("wcs_bathymetry fetches and returns a bathyRaster (live network)", {
  skip_if_no_internet()
  cache <- tempfile("wcs-test-"); dir.create(cache)

  bathy <- wcs_bathymetry(c(2, 3, 54, 55), cache_dir = cache, verbose = FALSE)

  expect_s3_class(bathy, "bathyRaster")
  expect_s3_class(bathy$raster, "stars")
  expect_equal(sf::st_crs(bathy$raster)$epsg, 4326L)
  expect_length(list.files(cache, pattern = "\\.tif$"), 1)
})

test_that("wcs_bathymetry hits cache on second call (live network)", {
  skip_if_no_internet()
  cache <- tempfile("wcs-test-"); dir.create(cache)

  # First call: downloads
  t1 <- system.time(
    wcs_bathymetry(c(2, 3, 54, 55), cache_dir = cache, verbose = FALSE)
  )
  # Second call: should be much faster (cache hit)
  t2 <- system.time(
    wcs_bathymetry(c(2, 3, 54, 55), cache_dir = cache, verbose = FALSE)
  )

  expect_true(t2["elapsed"] < t1["elapsed"])
})

test_that("wcs_bathymetry output plugs into basemap()", {
  skip_if_no_internet()
  cache <- tempfile("wcs-test-"); dir.create(cache)

  bathy <- wcs_bathymetry(c(2, 3, 54, 55), cache_dir = cache, verbose = FALSE)
  p <- basemap(
    c(2, 3, 54, 55),
    shapefiles = list(land = dd_land, glacier = NULL, bathy = bathy$raster),
    bathymetry = TRUE
  )
  expect_s3_class(p, "gg")
})

# bathy.style integration tests ------------------------------------------

test_that("define_bathy_style accepts wcs_emodnet_blues and abbreviations", {
  expect_equal(unname(define_bathy_style("wcs_emodnet_blues")), "bathy_rc")
  expect_equal(names(define_bathy_style("wcs_emodnet_blues")), "wcs_emodnet_blues")
  expect_equal(names(define_bathy_style("wemb")), "wcs_emodnet_blues")
  expect_equal(names(define_bathy_style("wemg")), "wcs_emodnet_grays")
})

test_that("basemap with bathy.style = wcs_emodnet_blues renders (live network)", {
  skip_if_no_internet()
  # Route the WCS cache to a fresh tempdir so we don't pollute the user's data dir.
  withr::with_options(
    list(ggOceanMaps.datapath = tempfile("wcs-basemap-")),
    {
      dir.create(getOption("ggOceanMaps.datapath"))
      p <- basemap(c(2, 3, 54, 55), bathy.style = "wcs_emodnet_blues", verbose = FALSE)
      expect_s3_class(p, "gg")
    }
  )
})

test_that("basemap rejects WCS styles on polar maps", {
  expect_error(
    basemap(60, bathy.style = "wcs_emodnet_blues", verbose = FALSE),
    "WCS bathymetry styles.*decimal-degree"
  )
})
