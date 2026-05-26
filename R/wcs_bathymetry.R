#' @title Download bathymetry from a WCS endpoint
#' @description Fetches gridded bathymetry data on demand from an OGC Web
#'   Coverage Service (WCS) and returns it as a \code{bathyRaster} object
#'   compatible with \code{\link{basemap}} and \code{\link{vector_bathymetry}}.
#'   Currently EMODnet is the supported source.
#' @param limits Numeric vector of length 4 giving the bounding box in decimal
#'   degrees as \code{c(xmin, xmax, ymin, ymax)}.
#' @param source Character. The WCS source to query. Currently only
#'   \code{"emodnet"} is supported. Partial matching applies.
#' @param coverage Character. Override the default coverage for the source
#'   (e.g. \code{"emodnet__mean_2022"} for the 2022-vintage EMODnet DTM).
#'   \code{NULL} uses the source default.
#' @param cache_dir Character. Directory in which downloaded GeoTIFFs are
#'   cached. Defaults to \code{getOption("ggOceanMaps.datapath")} or
#'   \code{tempdir()}.
#' @param force Logical. If \code{TRUE}, re-download even when a cached file
#'   exists.
#' @param max_area_deg2 Numeric. Maximum bounding-box area (in degree-squared)
#'   allowed before the function errors. Guards against accidentally
#'   downloading a very large area. Default \code{50}. Bounding boxes larger
#'   than the source's single-request cap are split into tiles and mosaicked
#'   automatically — see Details.
#' @param tile_size_deg Numeric. Edge length (in degrees) of the largest
#'   single-request tile. Defaults to \code{3} which keeps each EMODnet
#'   request comfortably under the server's ~98 MB read cap (EMODnet reads
#'   8-byte doubles internally, so a 4° tile already exceeds the cap).
#' @param timeout Numeric. HTTP timeout in seconds.
#' @param verbose Logical. Print download progress and informational messages.
#' @details EMODnet's WCS endpoint serves the European-waters bathymetric DTM
#'   at ~115 m native resolution (~0.00104°) in EPSG:4326 GeoTIFF format. The
#'   1°×1° tile around the North Sea is ~4 MB; a 5°×5° tile would be ~100 MB
#'   and hit the server's ~98 MB read cap. To handle larger bounding boxes,
#'   \code{wcs_bathymetry()} splits the request into tiles of at most
#'   \code{tile_size_deg} per axis, caches each tile, and mosaicks them via a
#'   GDAL virtual raster. Each tile is cached independently so subsequent
#'   overlapping requests reuse what's already on disk.
#'
#'   The returned object is a \code{bathyRaster} (same class returned by
#'   \code{\link{raster_bathymetry}} with \code{depths = NULL}) and can be
#'   slotted directly into \code{basemap(shapefiles = list(bathy = ...))} or
#'   passed to \code{\link{vector_bathymetry}}.
#'
#'   Citation requirements: EMODnet bathymetry is published under CC-BY and
#'   must be cited if used in figures. See
#'   \url{https://emodnet.ec.europa.eu/en/bathymetry}.
#' @return A \code{bathyRaster} object: a list with elements \code{raster}
#'   (a \code{\link[stars]{stars}} object with positive depth values) and
#'   \code{depth.invervals} (a length-2 numeric range).
#' @examples
#' \dontrun{
#'   bathy <- wcs_bathymetry(c(2, 3, 54, 55), source = "emodnet")
#'   basemap(c(2, 3, 54, 55),
#'           shapefiles = list(land = dd_land, glacier = NULL,
#'                             bathy = bathy$raster),
#'           bathymetry = TRUE)
#' }
#' @family create shapefiles
#' @author Mikko Vihtakari
#' @seealso \code{\link{raster_bathymetry}}, \code{\link{basemap}}
#' @export

wcs_bathymetry <- function(
    limits,
    source = "emodnet",
    coverage = NULL,
    cache_dir = NULL,
    force = FALSE,
    max_area_deg2 = 50,
    tile_size_deg = 3,
    timeout = 60,
    verbose = TRUE
) {

  ## Checks ----

  if(!(is.numeric(limits) && length(limits) == 4)) {
    stop("limits must be a numeric vector of length 4: c(xmin, xmax, ymin, ymax).")
  }

  bbox <- stats::setNames(limits, c("xmin", "xmax", "ymin", "ymax"))

  if(bbox["xmin"] >= bbox["xmax"]) {
    stop("limits[1] (xmin) must be less than limits[2] (xmax). Antimeridian crossing is not supported for WCS bathymetry yet.")
  }
  if(bbox["ymin"] >= bbox["ymax"]) {
    stop("limits[3] (ymin) must be less than limits[4] (ymax).")
  }

  area <- unname((bbox["xmax"] - bbox["xmin"]) * (bbox["ymax"] - bbox["ymin"]))
  if(area > max_area_deg2) {
    stop(sprintf(
      "Bounding box area (%.1f deg^2) exceeds max_area_deg2 (%.0f). Increase max_area_deg2 explicitly, or fetch the data in smaller tiles.",
      area, max_area_deg2
    ))
  }

  src <- wcs_source_info(match.arg(source, names(wcs_registry())), coverage)

  if(is.null(cache_dir)) {
    cache_dir <- getOption("ggOceanMaps.datapath", tempdir())
  }
  if(!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  ## Decide on tiling ----

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(old_timeout, timeout))

  dx <- unname(bbox["xmax"] - bbox["xmin"])
  dy <- unname(bbox["ymax"] - bbox["ymin"])

  if(dx <= tile_size_deg && dy <= tile_size_deg) {
    cache_file <- wcs_fetch_tile(src, bbox, cache_dir, force, verbose)
    ras <- stars::read_stars(cache_file, quiet = !verbose)
  } else {
    ## Tile and mosaic ----
    x_breaks <- sort(unique(c(seq(bbox["xmin"], bbox["xmax"], by = tile_size_deg), bbox["xmax"])))
    y_breaks <- sort(unique(c(seq(bbox["ymin"], bbox["ymax"], by = tile_size_deg), bbox["ymax"])))

    tiles <- expand.grid(
      i = seq_len(length(x_breaks) - 1L),
      j = seq_len(length(y_breaks) - 1L)
    )
    if(verbose) message(sprintf(
      "ggOceanMaps: bounding box exceeds %g deg per axis; fetching %d tile(s) from %s.",
      tile_size_deg, nrow(tiles), src$label
    ))

    tile_files <- vapply(seq_len(nrow(tiles)), function(k) {
      i <- tiles$i[k]; j <- tiles$j[k]
      tile_bbox <- stats::setNames(
        c(x_breaks[i], x_breaks[i + 1L], y_breaks[j], y_breaks[j + 1L]),
        c("xmin", "xmax", "ymin", "ymax")
      )
      wcs_fetch_tile(src, tile_bbox, cache_dir, force, verbose)
    }, character(1))

    # GDAL doesn't expand "~"; normalise paths before passing in.
    tile_files <- normalizePath(tile_files, mustWork = TRUE)
    vrt_path <- tempfile(fileext = ".vrt")
    on.exit(unlink(vrt_path), add = TRUE)
    sf::gdal_utils("buildvrt", source = tile_files, destination = vrt_path, quiet = !verbose)
    ras <- stars::read_stars(vrt_path, quiet = !verbose)
  }

  ## Convert to bathyRaster ----

  raster_bathymetry(ras, depths = NULL, verbose = verbose)
}


# Internal: fetch a single tile (returns path to cached GeoTIFF) ----------

wcs_fetch_tile <- function(src, bbox, cache_dir, force, verbose) {
  cache_file <- wcs_cache_path(cache_dir, src, bbox)

  if(!force && file.exists(cache_file)) {
    if(verbose) message("ggOceanMaps: using cached WCS tile: ", basename(cache_file))
    return(cache_file)
  }

  url <- wcs_build_url(src, bbox)
  if(verbose) message("ggOceanMaps: downloading ", basename(cache_file), " from ", src$label, " WCS...")

  result <- tryCatch(
    utils::download.file(url, destfile = cache_file, mode = "wb", quiet = !verbose),
    error = function(e) e,
    warning = function(w) w
  )

  if(inherits(result, c("error", "warning")) || !file.exists(cache_file) || file.size(cache_file) < 1000) {
    if(file.exists(cache_file)) unlink(cache_file)
    stop("WCS download failed. Check internet connectivity and that ", src$label, " is reachable. Original error: ", conditionMessage(result))
  }

  cache_file
}


# Internal: WCS source registry --------------------------------------------

wcs_registry <- function() {
  list(
    emodnet = list(
      label = "EMODnet",
      url = "https://ows.emodnet-bathymetry.eu/wcs",
      coverage = "emodnet__mean",
      version = "2.0.1",
      format = "image/tiff",
      crs = "EPSG:4326",
      # WCS axis labels (note: EMODnet uses Lat first)
      axis_lat = "Lat",
      axis_lon = "Long",
      native_res_deg = 0.00104167
    )
  )
}

wcs_source_info <- function(source, coverage = NULL) {
  src <- wcs_registry()[[source]]
  if(is.null(src)) stop("Unknown WCS source: ", source)
  if(!is.null(coverage)) src$coverage <- coverage
  src
}

wcs_build_url <- function(src, bbox) {
  q <- sprintf(
    "%s?SERVICE=WCS&VERSION=%s&REQUEST=GetCoverage&CoverageId=%s&SUBSET=%s(%f,%f)&SUBSET=%s(%f,%f)&FORMAT=%s",
    src$url, src$version, utils::URLencode(src$coverage),
    src$axis_lat, bbox["ymin"], bbox["ymax"],
    src$axis_lon, bbox["xmin"], bbox["xmax"],
    utils::URLencode(src$format)
  )
  q
}

wcs_cache_path <- function(cache_dir, src, bbox) {
  # Deterministic filename so repeated calls hit the cache.
  key <- sprintf(
    "wcs_%s_%s_%s_%s_%s_%s.tif",
    gsub("[^A-Za-z0-9]+", "-", src$coverage),
    formatC(bbox["xmin"], digits = 6, format = "f"),
    formatC(bbox["xmax"], digits = 6, format = "f"),
    formatC(bbox["ymin"], digits = 6, format = "f"),
    formatC(bbox["ymax"], digits = 6, format = "f"),
    gsub("\\.", "-", as.character(src$version))
  )
  file.path(cache_dir, key)
}
