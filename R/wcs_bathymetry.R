#' @title Download bathymetry from a WCS endpoint
#' @description Fetches gridded bathymetry data on demand from an OGC Web
#'   Coverage Service (WCS) and returns it as a \code{bathyRaster} object
#'   compatible with \code{\link{basemap}} and \code{\link{vector_bathymetry}}.
#'   Two sources are currently supported: \code{"emodnet"} (~115 m European
#'   waters) and \code{"etopo"} (1 arc-minute / ~1.85 km global).
#' @param limits Numeric vector of length 4 giving the bounding box in decimal
#'   degrees as \code{c(xmin, xmax, ymin, ymax)}.
#' @param source Character. The WCS source to query. One of \code{"emodnet"}
#'   (European waters, high-res) or \code{"etopo"} (global, 1 arc-minute).
#'   Partial matching applies.
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
#'   downloading a very large area. \code{NULL} uses the source default
#'   (50 for EMODnet, 2000 for ETOPO). Bounding boxes larger than the
#'   source's single-request cap are split into tiles and mosaicked
#'   automatically -- see Details.
#' @param tile_size_deg Numeric. Edge length (in degrees) of the largest
#'   single-request tile. \code{NULL} uses the source default (3 for
#'   EMODnet, 30 for ETOPO). EMODnet reads 8-byte doubles internally so a
#'   4-degree tile already exceeds its ~98 MB read cap; ETOPO is much coarser
#'   so larger tiles are fine.
#' @param downsample Non-negative integer. Number of grid cells to skip when
#'   requesting the raster. \code{0} (default) keeps the source resolution;
#'   \code{1} requests half the rows and columns; \code{n} requests every
#'   \code{(n+1)}-th cell. Downsampling is performed by the WCS server, reducing
#'   both transfer size and memory use.
#' @param timeout Positive numeric. Minimum HTTP timeout in seconds. A larger
#'   existing global \code{timeout} option is respected.
#' @param verbose Logical. Print download progress and informational messages.
#' @details \strong{EMODnet} serves the European-waters bathymetric DTM at
#'   ~115 m native resolution (~0.00104 deg) in EPSG:4326 GeoTIFF format. The
#'   1x1 deg tile around the North Sea is ~4 MB; a 5x5 deg tile would be
#'   ~100 MB and hit the server's ~98 MB read cap. Coverage is European
#'   regional seas only (~-36 to 43 lon, ~15 to 90 lat).
#'
#'   \strong{ETOPO} (ETOPO1 Ice Surface, served by NOAA NCEI) is a global
#'   1 arc-minute (~1.85 km) topo-bathy grid in EPSG:4326. Useful when EMODnet
#'   has no coverage. NCEI returns the GeoTIFF inside a multipart/related MIME
#'   envelope; \code{wcs_bathymetry()} extracts the binary part transparently.
#'
#'   To handle bounding boxes larger than the source's single-request cap,
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
#'   must be cited if used in figures
#'   (\url{https://emodnet.ec.europa.eu/en/bathymetry}). ETOPO1 also requires
#'   citation (Amante & Eakins 2009, NOAA NGDC; see
#'   \url{https://www.ncei.noaa.gov/products/etopo-global-relief-model}).
#' @return A \code{bathyRaster} object: a list with elements \code{raster}
#'   (a \code{\link[stars]{read_stars}} object with positive depth values) and
#'   \code{depth.invervals} (a length-2 numeric range).
#' @examples
#' \dontrun{
#'   # European waters, high resolution
#'   bathy <- wcs_bathymetry(c(2, 3, 54, 55), source = "emodnet")
#'   basemap(c(2, 3, 54, 55),
#'           shapefiles = list(land = dd_land, glacier = NULL,
#'                             bathy = bathy$raster),
#'           bathymetry = TRUE)
#'
#'   # Global coverage (Hawaii -- outside EMODnet)
#'   bathy <- wcs_bathymetry(c(-160, -154, 18, 23), source = "etopo")
#'   basemap(c(-160, -154, 18, 23),
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
    max_area_deg2 = NULL,
    tile_size_deg = NULL,
    downsample = 0,
    timeout = 60,
    verbose = TRUE
) {

  ## Checks ----

  if(!(is.numeric(limits) && length(limits) == 4)) {
    stop("limits must be a numeric vector of length 4: c(xmin, xmax, ymin, ymax).")
  }
  if(anyNA(limits) || any(!is.finite(limits))) {
    stop("limits must contain four finite, non-missing values.")
  }

  bbox <- stats::setNames(limits, c("xmin", "xmax", "ymin", "ymax"))

  if(bbox["xmin"] >= bbox["xmax"]) {
    stop("limits[1] (xmin) must be less than limits[2] (xmax). Antimeridian crossing is not supported for WCS bathymetry yet.")
  }
  if(bbox["ymin"] >= bbox["ymax"]) {
    stop("limits[3] (ymin) must be less than limits[4] (ymax).")
  }
  if(any(bbox[c("xmin", "xmax")] < -180 | bbox[c("xmin", "xmax")] > 180)) {
    stop("Longitude limits must be between -180 and 180 degrees.")
  }
  if(any(bbox[c("ymin", "ymax")] < -90 | bbox[c("ymin", "ymax")] > 90)) {
    stop("Latitude limits must be between -90 and 90 degrees.")
  }

  if(!(is.character(source) && length(source) == 1L && !is.na(source) && nzchar(source))) {
    stop("source must be one non-empty character value.")
  }
  if(!is.null(coverage) && !(is.character(coverage) && length(coverage) == 1L &&
                             !is.na(coverage) && nzchar(coverage))) {
    stop("coverage must be NULL or one non-empty character value.")
  }
  if(!(is.logical(force) && length(force) == 1L && !is.na(force))) {
    stop("force must be TRUE or FALSE.")
  }
  if(!(is.logical(verbose) && length(verbose) == 1L && !is.na(verbose))) {
    stop("verbose must be TRUE or FALSE.")
  }
  if(!(is.numeric(downsample) && length(downsample) == 1L && !is.na(downsample) &&
       is.finite(downsample) && downsample >= 0 && downsample == as.integer(downsample))) {
    stop("downsample must be one non-negative integer.")
  }
  downsample <- as.integer(downsample)
  if(!(is.numeric(timeout) && length(timeout) == 1L && !is.na(timeout) &&
       is.finite(timeout) && timeout > 0)) {
    stop("timeout must be one positive finite number.")
  }

  src <- wcs_source_info(match.arg(source, names(wcs_registry())), coverage)

  ## Resolve source-dependent defaults ----
  if(is.null(max_area_deg2)) {
    max_area_deg2 <- if(!is.null(src$default_max_area_deg2)) src$default_max_area_deg2 else 50
  }
  if(is.null(tile_size_deg)) {
    tile_size_deg <- if(!is.null(src$default_tile_size_deg)) src$default_tile_size_deg else 3
  }
  if(!(is.numeric(max_area_deg2) && length(max_area_deg2) == 1L &&
       !is.na(max_area_deg2) && is.finite(max_area_deg2) && max_area_deg2 > 0)) {
    stop("max_area_deg2 must be one positive finite number.")
  }
  if(!(is.numeric(tile_size_deg) && length(tile_size_deg) == 1L &&
       !is.na(tile_size_deg) && is.finite(tile_size_deg) && tile_size_deg > 0)) {
    stop("tile_size_deg must be one positive finite number.")
  }

  ## Check whether the bbox lies entirely outside the source's known coverage ----
  ## (Do this before the area guard so the user gets a useful diagnostic, not
  ##  a confusing "area too large" message for an area in the wrong ocean.)
  if(!is.null(src$extent)) {
    ext <- src$extent
    if(bbox["xmin"] > ext[2] || bbox["xmax"] < ext[1] ||
       bbox["ymin"] > ext[4] || bbox["ymax"] < ext[3]) {
      stop(sprintf(paste0(
        "Bounding box (%.1f\u00b0 to %.1f\u00b0 lon, %.1f\u00b0 to %.1f\u00b0 lat) lies ",
        "entirely outside the approximate coverage of %s ",
        "(\u2248%.0f\u00b0 to %.0f\u00b0 lon, %.0f\u00b0 to %.0f\u00b0 lat).\n",
        "For global bathymetry coverage, download GEBCO or ETOPO data locally and use ",
        "raster_bathymetry() + vector_bathymetry() instead, or wait for a global WCS ",
        "source to be added to ggOceanMaps."
      ),
        unname(bbox["xmin"]), unname(bbox["xmax"]),
        unname(bbox["ymin"]), unname(bbox["ymax"]),
        src$label,
        ext[1], ext[2], ext[3], ext[4]
      ))
    }
  }

  area <- unname((bbox["xmax"] - bbox["xmin"]) * (bbox["ymax"] - bbox["ymin"]))
  if(area > max_area_deg2) {
    stop(sprintf(
      "Bounding box area (%.1f deg^2) exceeds max_area_deg2 (%.0f). Increase max_area_deg2 explicitly, or fetch the data in smaller tiles.",
      area, max_area_deg2
    ))
  }

  if(is.null(cache_dir)) {
    cache_dir <- getOption("ggOceanMaps.datapath", tempdir())
  }
  if(!(is.character(cache_dir) && length(cache_dir) == 1L &&
       !is.na(cache_dir) && nzchar(cache_dir))) {
    stop("cache_dir must be one non-empty character path.")
  }
  if(!dir.exists(cache_dir)) {
    created <- dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if(!created && !dir.exists(cache_dir)) {
      stop("Could not create WCS cache directory: ", cache_dir)
    }
  }

  ## Decide on tiling ----

  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = max(old_timeout, timeout))

  dx <- unname(bbox["xmax"] - bbox["xmin"])
  dy <- unname(bbox["ymax"] - bbox["ymin"])

  if(dx <= tile_size_deg && dy <= tile_size_deg) {
    cache_file <- wcs_fetch_tile(src, bbox, cache_dir, force, verbose, downsample)
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
      wcs_fetch_tile(src, tile_bbox, cache_dir, force, verbose, downsample)
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

wcs_fetch_tile <- function(src, bbox, cache_dir, force, verbose, downsample = 0L) {
  cache_file <- wcs_cache_path(cache_dir, src, bbox, downsample)

  if(!force && file.exists(cache_file)) {
    if(.is_valid_tiff(cache_file)) {
      if(verbose) message("ggOceanMaps: using cached WCS tile: ", basename(cache_file))
      return(cache_file)
    }
    if(verbose) message("ggOceanMaps: removing invalid cached WCS tile: ", basename(cache_file))
    unlink(cache_file)
  }

  url <- wcs_build_url(src, bbox, downsample)
  if(verbose) message("ggOceanMaps: downloading ", basename(cache_file), " from ", src$label, " WCS...")

  download_file <- tempfile(
    pattern = paste0(basename(cache_file), "-"),
    tmpdir = cache_dir
  )
  on.exit(unlink(download_file), add = TRUE)

  result <- tryCatch(
    wcs_download_file(url, destfile = download_file, quiet = !verbose),
    error = function(e) e,
    warning = function(w) w
  )

  if(inherits(result, c("error", "warning")) || !file.exists(download_file) || file.size(download_file) < 1000) {
    err_detail <- if(inherits(result, "condition")) paste0(" Original error: ", conditionMessage(result)) else ""
    stop("WCS download failed. Check internet connectivity and that ", src$label, " is reachable.", err_detail)
  }

  ## Some servers (NCEI/ArcGIS) wrap the GeoTIFF in a multipart/related MIME
  ## envelope. Extract the binary part in-place before validation.
  if(isTRUE(src$multipart)) {
    extracted <- .extract_tiff_from_multipart(download_file)
    if(!extracted) {
      stop(sprintf(
        "Could not extract a GeoTIFF from the multipart response from %s. The requested area may be outside coverage.",
        src$label
      ))
    }
  }

  if(!.is_valid_tiff(download_file)) {
    stop(sprintf(paste0(
      "Downloaded file from %s is not a valid GeoTIFF ",
      "(the server likely returned an XML error document). ",
      "The requested area may be outside the source's coverage. ",
      "See %s for coverage information."
    ), src$label, src$url))
  }

  if(file.exists(cache_file)) unlink(cache_file)
  if(!file.rename(download_file, cache_file)) {
    # Another process may have completed the same tile first.
    if(file.exists(cache_file) && .is_valid_tiff(cache_file)) return(cache_file)
    stop("Could not move the validated WCS tile into the cache: ", cache_file)
  }

  cache_file
}

wcs_download_file <- function(url, destfile, quiet) {
  utils::download.file(url, destfile = destfile, mode = "wb", quiet = quiet)
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
      native_res_deg = 0.00104167,
      # Approximate geographic extent c(xmin, xmax, ymin, ymax) in decimal degrees.
      # Used to reject clearly out-of-range requests before hitting the network.
      # EMODnet covers European regional seas only (North Atlantic -> Arctic -> Black Sea).
      extent = c(-36, 43, 15, 90),
      # Server returns raw GeoTIFF (no multipart envelope)
      multipart = FALSE,
      # Source-specific size guards
      default_max_area_deg2 = 50,
      default_tile_size_deg = 3,
      # GeoServer follows OGC scaling semantics: values below one reduce size.
      scale_factor = function(downsample) 1 / (downsample + 1)
    ),
    etopo = list(
      label = "ETOPO1 (NOAA NCEI)",
      url = "https://gis.ngdc.noaa.gov/arcgis/services/DEM_mosaics/ETOPO1_ice_surface/ImageServer/WCSServer",
      coverage = "Coverage1",
      version = "2.0.1",
      format = "image/tiff",
      crs = "EPSG:4326",
      # ArcGIS Server uses lowercase 'y' / 'x' axis labels
      axis_lat = "y",
      axis_lon = "x",
      native_res_deg = 0.01667,  # 1 arc-minute
      # Global coverage
      extent = c(-180, 180, -90, 90),
      # NCEI wraps the GeoTIFF in a multipart/related MIME envelope
      multipart = TRUE,
      # Much coarser than EMODnet -> larger areas / tiles are fine
      default_max_area_deg2 = 2000,
      default_tile_size_deg = 30,
      # ArcGIS WCS uses the reciprocal convention: values above one reduce size.
      scale_factor = function(downsample) downsample + 1
    )
  )
}

wcs_source_info <- function(source, coverage = NULL) {
  src <- wcs_registry()[[source]]
  if(is.null(src)) stop("Unknown WCS source: ", source)
  src$name <- source
  if(!is.null(coverage)) src$coverage <- coverage
  src
}

wcs_build_url <- function(src, bbox, downsample = 0L) {
  q <- sprintf(
    "%s?SERVICE=WCS&VERSION=%s&REQUEST=GetCoverage&CoverageId=%s&SUBSET=%s(%f,%f)&SUBSET=%s(%f,%f)&FORMAT=%s",
    src$url, src$version, utils::URLencode(src$coverage, reserved = TRUE),
    src$axis_lat, bbox["ymin"], bbox["ymax"],
    src$axis_lon, bbox["xmin"], bbox["xmax"],
    utils::URLencode(src$format, reserved = TRUE)
  )
  if(downsample > 0L) {
    q <- paste0(q, "&SCALEFACTOR=", format(src$scale_factor(downsample), scientific = FALSE, trim = TRUE))
  }
  q
}

wcs_cache_path <- function(cache_dir, src, bbox, downsample = 0L) {
  # Deterministic filename so repeated calls hit the cache.
  key <- sprintf(
    "wcs_%s_%s_%s_%s_%s_%s_%s_ds%s.tif",
    src$name,
    gsub("[^A-Za-z0-9]+", "-", src$coverage),
    formatC(bbox["xmin"], digits = 6, format = "f"),
    formatC(bbox["xmax"], digits = 6, format = "f"),
    formatC(bbox["ymin"], digits = 6, format = "f"),
    formatC(bbox["ymax"], digits = 6, format = "f"),
    gsub("\\.", "-", as.character(src$version)),
    downsample
  )
  file.path(cache_dir, key)
}


# Internal: extract a GeoTIFF from a multipart/related MIME envelope ------
# Used for servers like NOAA NCEI that wrap the binary in a multipart message.
# Locates the "Content-Type: image/tiff" header, skips past the blank line,
# and copies bytes up to (but not including) the next boundary marker.
# Returns TRUE on success (file is overwritten in place with the raw TIFF),
# FALSE if the multipart structure could not be parsed.

.extract_tiff_from_multipart <- function(path) {
  rb <- readBin(path, raw(), n = file.size(path))

  first_crlf <- grepRaw(as.raw(c(0x0D, 0x0A)), rb, all = FALSE)
  first_lf <- grepRaw(as.raw(0x0A), rb, all = FALSE)
  line_ends <- c(first_crlf, first_lf)
  if(length(line_ends) == 0L) return(FALSE)
  first_line_end <- min(line_ends)
  boundary <- rb[seq_len(first_line_end - 1L)]
  if(length(boundary) < 3L || !identical(boundary[1:2], as.raw(c(0x2D, 0x2D)))) {
    return(FALSE)
  }

  ct_marker <- charToRaw("Content-Type: image/tiff")
  ct_pos <- grepRaw(ct_marker, rb, all = FALSE)
  if(length(ct_pos) == 0L) return(FALSE)
  ct_start <- ct_pos[1L]

  # Header block terminator: \r\n\r\n or \n\n
  tail_rb <- rb[ct_start:length(rb)]
  blank_crlf <- grepRaw(as.raw(c(0x0D, 0x0A, 0x0D, 0x0A)), tail_rb, all = FALSE)
  blank_lf <- grepRaw(as.raw(c(0x0A, 0x0A)), tail_rb, all = FALSE)
  if(length(blank_crlf) > 0L) {
    body_start <- ct_start + blank_crlf[1L] + 3L  # past the four bytes
  } else if(length(blank_lf) > 0L) {
    body_start <- ct_start + blank_lf[1L] + 1L    # past the two bytes
  } else {
    return(FALSE)
  }

  # Match the complete MIME boundary, not a generic "\n--" byte sequence that
  # can occur naturally inside compressed TIFF data.
  body_rb <- rb[body_start:length(rb)]
  bdy_marker <- c(as.raw(0x0A), boundary)
  bdy <- grepRaw(bdy_marker, body_rb, all = FALSE)
  if(length(bdy) == 0L) {
    return(FALSE)
  }
  body_end <- body_start + bdy[1L] - 2L  # one before the \n
  # Strip preceding \r if present (\r\n line ending)
  if(body_end >= 1L && rb[body_end] == as.raw(0x0D)) body_end <- body_end - 1L

  if(body_end <= body_start) return(FALSE)

  writeBin(rb[body_start:body_end], path)
  TRUE
}


# Internal: validate a downloaded file has TIFF magic bytes ---------------
# Returns FALSE for XML error documents, HTML pages, or truncated files.

.is_valid_tiff <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  magic <- readBin(con, raw(), n = 4L)
  if(length(magic) < 4L) return(FALSE)
  # Little-endian TIFF / BigTIFF: "II" (0x49 0x49) then 0x2A or 0x2B
  le <- magic[1L] == as.raw(0x49L) && magic[2L] == as.raw(0x49L) &&
        (magic[3L] == as.raw(0x2AL) || magic[3L] == as.raw(0x2BL))
  # Big-endian TIFF / BigTIFF: "MM" followed by 0x002A or 0x002B.
  be <- magic[1L] == as.raw(0x4DL) && magic[2L] == as.raw(0x4DL) &&
        magic[3L] == as.raw(0x00L) &&
        (magic[4L] == as.raw(0x2AL) || magic[4L] == as.raw(0x2BL))
  le || be
}
