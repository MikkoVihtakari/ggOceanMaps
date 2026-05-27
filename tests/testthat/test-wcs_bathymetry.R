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

# Coverage extent and TIFF-validation tests (no network required) ---------

test_that("wcs_bathymetry errors for bbox entirely outside EMODnet coverage", {
  # Indonesian/Pacific waters — clearly outside EMODnet's European coverage
  expect_error(
    wcs_bathymetry(c(110, 120, -20, 30)),
    "entirely outside.*EMODnet"
  )
  expect_error(
    wcs_bathymetry(c(115, 120, -20, 30)),
    "entirely outside.*EMODnet"
  )
  # Pacific meridian — also outside
  expect_error(
    wcs_bathymetry(c(160, 170, -50, -40)),
    "entirely outside.*EMODnet"
  )
})

test_that("wcs_bathymetry does NOT error for bbox inside EMODnet coverage (network not called)", {
  # This just checks the extent logic — the request would need a network call
  # to actually proceed, so we expect *any* error *other* than the extent error.
  # We trigger the cache_dir initialisation stage so the extent check must pass.
  result <- tryCatch(
    wcs_bathymetry(c(2, 3, 54, 55), cache_dir = tempfile("no-net-"), timeout = 0.001),
    error = function(e) e
  )
  # Whatever happens next (timeout, connection refused, etc.) is fine;
  # the important thing is no "entirely outside" error.
  if(inherits(result, "error")) {
    expect_false(grepl("entirely outside", conditionMessage(result)))
  }
})

test_that(".is_valid_tiff correctly identifies TIFF and non-TIFF files", {
  # Write a minimal little-endian TIFF header
  tiff_file <- tempfile(fileext = ".tif")
  writeBin(as.raw(c(0x49, 0x49, 0x2A, 0x00, 0x00, 0x00, 0x00, 0x00)), tiff_file)
  expect_true(.is_valid_tiff(tiff_file))

  # Write an XML error document (as EMODnet WCS returns for bad requests)
  xml_file <- tempfile(fileext = ".xml")
  writeLines('<?xml version="1.0"?><ows:ExceptionReport/>', xml_file)
  expect_false(.is_valid_tiff(xml_file))

  # Write a big-endian TIFF header
  tiff_be_file <- tempfile(fileext = ".tif")
  writeBin(as.raw(c(0x4D, 0x4D, 0x00, 0x2A, 0x00, 0x00, 0x00, 0x08)), tiff_be_file)
  expect_true(.is_valid_tiff(tiff_be_file))

  unlink(c(tiff_file, xml_file, tiff_be_file))
})

# ETOPO source tests -----------------------------------------------------

test_that("wcs_registry exposes the etopo source with expected fields", {
  reg <- wcs_registry()
  expect_true("etopo" %in% names(reg))
  expect_equal(reg$etopo$coverage, "Coverage1")
  expect_equal(reg$etopo$axis_lat, "y")
  expect_equal(reg$etopo$axis_lon, "x")
  expect_true(reg$etopo$multipart)
  expect_equal(reg$etopo$extent, c(-180, 180, -90, 90))
})

test_that("ETOPO default_max_area_deg2 (2000) > EMODnet default (50)", {
  # Direct registry sanity: a 30 deg^2 bbox should be fine for ETOPO defaults,
  # whereas a >50 deg^2 bbox would error for EMODnet without max_area_deg2.
  reg <- wcs_registry()
  expect_gt(reg$etopo$default_max_area_deg2, 100)
  expect_lte(reg$emodnet$default_max_area_deg2, 100)

  # And the same area bbox triggers the EMODnet area guard but not ETOPO's,
  # without any network call. We use a bbox that's inside EMODnet coverage so
  # the extent check passes, then the area guard fires for EMODnet only.
  expect_error(
    wcs_bathymetry(c(0, 10, 50, 60), source = "emodnet"),  # 100 deg^2 > 50
    "exceeds max_area_deg2"
  )
  # ETOPO covers Hawaii (outside EMODnet), 5x5=25 deg^2 — neither guard fires.
  # Stop at the timeout/network stage which is fine.
  result <- tryCatch(
    wcs_bathymetry(c(-160, -155, 18, 23), source = "etopo",
                   cache_dir = tempfile("no-net-"), timeout = 0.001),
    error = function(e) e
  )
  if(inherits(result, "error")) {
    expect_false(grepl("exceeds max_area_deg2|entirely outside",
                       conditionMessage(result)))
  }
})

test_that("define_bathy_style accepts wcs_etopo_* and abbreviations", {
  expect_equal(unname(define_bathy_style("wcs_etopo_blues")), "bathy_rc")
  expect_equal(names(define_bathy_style("wcs_etopo_blues")), "wcs_etopo_blues")
  expect_equal(names(define_bathy_style("wceb")), "wcs_etopo_blues")
  expect_equal(names(define_bathy_style("wceg")), "wcs_etopo_grays")
})

test_that("wcs_build_url respects per-source axis labels (etopo uses y/x)", {
  src <- wcs_source_info("etopo")
  url <- wcs_build_url(src, c(xmin = -158, xmax = -156, ymin = 20, ymax = 22))
  expect_match(url, "gis\\.ngdc\\.noaa\\.gov")
  expect_match(url, "CoverageId=Coverage1")
  expect_match(url, "SUBSET=y\\(20")
  expect_match(url, "SUBSET=x\\(-158")
})

test_that(".extract_tiff_from_multipart pulls TIFF out of MIME envelope", {
  # Build a synthetic multipart envelope: GML part + TIFF part
  gml_part <- charToRaw(paste0(
    "--wcs\r\n",
    "Content-Type: text/xml\r\n",
    "Content-ID: GML-Part\r\n\r\n",
    "<dummy/>\r\n"
  ))
  tiff_header <- charToRaw(paste0(
    "--wcs\r\n",
    "Content-Type: image/tiff\r\n",
    "Content-ID: 1.tif\r\n\r\n"
  ))
  fake_tiff <- as.raw(c(0x49, 0x49, 0x2A, 0x00,
                        0x10, 0x00, 0x00, 0x00,  # arbitrary "body" bytes
                        0xDE, 0xAD, 0xBE, 0xEF))
  trailer <- charToRaw("\r\n--wcs--\r\n")

  envelope <- c(gml_part, tiff_header, fake_tiff, trailer)
  path <- tempfile(fileext = ".tif")
  writeBin(envelope, path)

  expect_true(.extract_tiff_from_multipart(path))

  # File should now contain only the fake_tiff bytes
  out <- readBin(path, raw(), n = file.size(path))
  expect_identical(out, fake_tiff)
  expect_true(.is_valid_tiff(path))

  unlink(path)
})

test_that(".extract_tiff_from_multipart returns FALSE for malformed input", {
  path <- tempfile(fileext = ".bin")
  writeBin(charToRaw("No Content-Type header here, just text."), path)
  expect_false(.extract_tiff_from_multipart(path))
  unlink(path)
})

# Live network test (ETOPO) ----------------------------------------------

test_that("wcs_bathymetry fetches a global tile from ETOPO (live network)", {
  skip_if_no_internet()
  cache <- tempfile("wcs-etopo-"); dir.create(cache)

  # Hawaii — clearly outside EMODnet, requires ETOPO/global coverage
  bathy <- wcs_bathymetry(c(-160, -154, 18, 23), source = "etopo",
                          cache_dir = cache, verbose = FALSE)

  expect_s3_class(bathy, "bathyRaster")
  expect_s3_class(bathy$raster, "stars")
  expect_equal(sf::st_crs(bathy$raster)$epsg, 4326L)
  # Expect at least one cached file (1 tile, since 6×5° < 30° tile size)
  expect_gte(length(list.files(cache, pattern = "\\.tif$")), 1L)
})
