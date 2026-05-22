#' @title Create a land polygon from a raster bathymetry
#' @description Extracts the land area from a \code{bathyRaster} object produced
#'   by \code{\link{raster_bathymetry}} and returns it as an
#'   \link[sf:st_polygon]{sf} polygon layer suitable for use in the
#'   \code{shapefiles} argument of \code{\link{basemap}}. Warning: processing
#'   may take a long time if the bathymetry raster is large.
#' @param bathy A \code{bathyRaster} object from \code{\link{raster_bathymetry}}.
#'   Land cells must be detectable either as \code{NA} (the default when
#'   \code{depths = NULL} or \code{estimate.land = FALSE}) or as the factor
#'   level \code{"land"} (when \code{estimate.land = TRUE}).
#' @inheritParams vector_bathymetry
#' @details The \code{drop.crumbs} and \code{remove.holes} arguments can be used
#'   to reduce the resulting file size. The \code{smooth} argument removes the
#'   pixelated contours but increases file size and biases the polygon with
#'   respect to the underlying raster.
#'
#'   Use \code{vector_land()} together with \code{\link{vector_bathymetry}} to
#'   build a matched land + bathymetry pair from a single source raster (e.g.
#'   GEBCO, ETOPO, IBCAO) — see the example.
#' @return An \link[sf:st_polygon]{sf} object containing the land polygons in
#'   the same projection as \code{bathy$raster}.
#' @author Mikko Vihtakari
#' @family create shapefiles
#' @seealso \code{\link{raster_bathymetry}}, \code{\link{vector_bathymetry}}
#' @export

vector_land <- function(bathy, drop.crumbs = NULL, remove.holes = NULL, smooth = FALSE) {

  ## Checks ----

  if(!inherits(bathy, "bathyRaster")) {
    stop("bathy has to be output from the raster_bathymetry function.")
  }
  if(is.na(sf::st_crs(bathy$raster))) {
    stop("bathy does not contain coordinate reference information.")
  }
  if(!inherits(bathy$raster, c("stars", "stars_proxy"))) {
    stop("bathy$raster has to be a stars object.")
  }

  if(!is.null(drop.crumbs)) {
    if(!(is.vector(drop.crumbs) & inherits(drop.crumbs, c("numeric", "integer")) & length(drop.crumbs) == 1)) {
      stop("The drop.crumbs parameter has to be a single value.")
    }
  }

  pb <- utils::txtProgressBar(min = 0, max = 6, initial = 0, style = 3)

  ## Build a binary land mask ----

  ras <- bathy$raster
  vals <- ras[[1]]

  if(is.factor(vals)) {
    land_mask <- as.character(vals) == "land"
  } else {
    land_mask <- is.na(vals)
  }
  land_mask[is.na(land_mask)] <- FALSE

  if(!any(land_mask)) {
    stop("No land cells detected in bathy$raster. Either call raster_bathymetry() with depths = NULL on a raster that includes land, or with estimate.land = TRUE.")
  }

  land_ras <- ras
  land_ras[[1]] <- ifelse(land_mask, 1L, NA_integer_)
  dim(land_ras[[1]]) <- dim(ras[[1]])

  utils::setTxtProgressBar(pb, 1)

  ## Polygonization ----

  pol <- sf::st_as_sf(land_ras, as_points = FALSE, merge = TRUE)

  utils::setTxtProgressBar(pb, 2)

  pol <- sf::st_make_valid(pol)
  if(!all(sf::st_is_valid(pol))) {
    stop("The initial geometry validation did not work.")
  }

  utils::setTxtProgressBar(pb, 3)

  ## Drop crumbs / fill holes ----

  if(!is.null(drop.crumbs)) {
    pol <- pol[sf::st_area(pol) > units::set_units(drop.crumbs, "km^2", mode = "standard"), ]
  }

  utils::setTxtProgressBar(pb, 4)

  if(!is.null(remove.holes)) {
    pol <- smoothr::fill_holes(pol, units::set_units(remove.holes, "km^2", mode = "standard"))
  }

  utils::setTxtProgressBar(pb, 5)

  ## Smooth ----

  if(smooth) {
    pol <- smoothr::smooth(pol, method = "ksmooth")
    if(!all(sf::st_is_valid(pol))) pol <- sf::st_make_valid(pol)
    pol <- sf::st_simplify(pol, preserveTopology = TRUE)
  }

  if(!all(sf::st_is_valid(pol))) pol <- sf::st_make_valid(pol)

  utils::setTxtProgressBar(pb, 6)

  ## Return ----

  pol <- sf::st_sf(land = TRUE, geometry = sf::st_geometry(pol))
  pol
}
