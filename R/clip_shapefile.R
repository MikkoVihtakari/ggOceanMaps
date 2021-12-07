#' @title Clip a shapefile (SpatialPolygon) using a bounding area
#' @description Clips an area from a larger shape file (\link[sp]{SpatialPolygons}).
#' @param x Original shape file to be clipped as a an \link[sp]{sp} or \link[sf]{sf} polygons object. Required. Must contain \code{\link[sp:is.projected]{CRS}} information.
#' @param limits The constraining area used to clip \code{x}. Required. Either a numeric vector of length 4 or a \link[sp]{SpatialPolygons} object. The first element of the numeric vector defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. The \link[sp]{SpatialPolygons} object must contain \code{\link[sp:is.projected]{CRS}} information. See details.
#' @param proj.limits The \code{\link[sp:is.projected]{CRS}} projection attributes for \code{limits} as character string (will be passed to \code{\link[sp]{CRS}}). Use the PROJ6 format. Defaults to decimal degrees (see Usage).
#' @param simplify Should the \code{x} geometry be simplified before clipping? Useful to make the function faster for large shape files. Uses \code{rgeos::gSimplify} function.
#' @param tol Numerical tolerance value to be used for simplification. See \code{?rgeos::gSimplify}.
#' @param return.boundary logical. If \code{TRUE} returns the clip boundary together with the shapefile
#' @details The function uses the \code{rgeos::gIntersection} function to clip smaller \link[sp]{SpatialPolygons} from larger ones. The clip area is constrained by either a numeric vector or \link[sp]{SpatialPolygons} object in the \code{limits} argument. One of these arguments must be given. Defining \code{limits} by a \link[sp]{SpatialPolygons} object gives greater freedom for the clip area as the area does not have to be rectangular.
#' @return Clipped \code{\link[sp]{SpatialPolygons}} object. If \code{return.boundary = TRUE}, a list containing the shapefile together with the clip boundary.
#' @keywords internal
#' @family create shapefiles
#' @import sp
#' @importFrom rgeos gIntersection gIntersects gSimplify
#' @importFrom sf as_Spatial
#' @importFrom methods slot slot<-
#' @importFrom grDevices chull
#' @author Mikko Vihtakari with a solution from \href{https://stackoverflow.com/questions/15881455/how-to-clip-worldmap-with-polygon-in-r}{Simon O'Hanlon, Roger Bivand/SO community}
#' @export

# Test parameters
# x = shapefiles$land; proj.limits = convert_crs(4326); return.boundary = TRUE
# proj.limits = convert_crs(4326); simplify = FALSE; tol = 60; return.boundary = FALSE
clip_shapefile <- function(x, limits, proj.limits = convert_crs(4326), simplify = FALSE, tol = 60, return.boundary = FALSE) {

  ## Checks

  if("sf" %in% class(x)) {
    x <- sf::as_Spatial(x)
  }

  ## Projection

  x_proj <- suppressWarnings(sp::proj4string(x))

  if(is.na(x_proj)) stop("crs for x is missing. Define the projection attributes and try again.")

  ## Clip boundary

  if(class(limits) == "SpatialPolygonsDataFrame" | class(limits) == "SpatialPolygons") {
    proj.limits <- suppressWarnings(sp::proj4string(limits))
    clip_boundary <- limits
  } else {
    if(!is.numeric(limits)) stop("limits have to be numeric, SpatialPolygonsDataFrame or SpatialPolygons object")
    if(length(limits) == 1) {
      bd <- data.frame(lon = seq(-180, 180, by = 0.5), lat = limits)
      bd <- transform_coord(bd, proj.out = x_proj)
      ch <- grDevices::chull(bd[[2]], bd[[1]])
      coords <- as.matrix(bd[c(ch, ch[1]), 1:2])
      clip_boundary <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords)), ID = 1)))

      if(!rgeos::gIsValid(clip_boundary)) stop("Invalid geomethry due to clip_shapefile. Add the buffering script.")

      suppressWarnings(sp::proj4string(clip_boundary) <- x_proj)

    } else if(length(limits) != 4) {
      stop("the length of limits vector has to be 4. See limits argument")
    } else {
      clip_boundary <- sp::Polygon(matrix(c(limits[1], limits[3], limits[1], limits[4], limits[2], limits[4], limits[2], limits[3], limits[1], limits[3]), ncol = 2, byrow = TRUE))
      clip_boundary <- sp::SpatialPolygons(list(sp::Polygons(list(clip_boundary), ID = "clip_boundary")), proj4string = suppressWarnings(sp::CRS(proj.limits)))
    }
  }

  ## Check that the projections match

  if(proj.limits != x_proj) {
    clip_boundary <- sp::spTransform(clip_boundary, suppressWarnings(sp::CRS(x_proj)))
  }

  ## Simplify bathymetry.

  if(simplify) {
    x <- rgeos::gSimplify(x, tol = tol)
  }

  ## Clipping the bathymetry (using a bypass scavenged from here: https://stackoverflow.com/questions/15881455/how-to-clip-worldmap-with-polygon-in-r)
  ## Sometimes rgeos::gIntersection gets confused when the resulting clipped SpatialPolygon contains other shapes than polygons. The bypass fixes this problem, but takes a longer time to complete than the regular method. Therefore two methods
  ## Alternatives here are:
  ## system.time(sf::as_Spatial(st_crop(sf::st_as_sf(x), sf::st_as_sf(clip_boundary)))) # Requires tuning to make circular crop work
  ## system.time(raster::crop(x, clip_boundary))
  ## Both seem slower than the chosen method

  error_test <- suppressWarnings(quiet(try(rgeos::gIntersection(x, clip_boundary, byid = TRUE), silent = TRUE)))

  if(class(error_test) == "try-error") {
    shapefile <- suppressWarnings(rgeos::gIntersection(x, clip_boundary, byid = TRUE, drop_lower_td = TRUE, checkValidity = 0L))
  } else if(is.null(error_test)) {
    shapefile <- x[-1:-length(x),]
  } else {
    shapefile <- error_test
  }

  if(class(x) == "SpatialPolygonsDataFrame" & length(shapefile) > 0) {
    ids <- sapply(slot(shapefile, "polygons"), function(x) slot(x, "ID"))
    ids <- select_element(strsplit(ids, " "), 1)

    if(ncol(x@data) == 1) {
      tmp.df <- data.frame(x@data[ids,])
      names(tmp.df) <- names(x@data)
    } else {
      tmp.df <- x@data[ids,]
    }

    shapefile <- sp::SpatialPolygonsDataFrame(shapefile, tmp.df, match.ID = FALSE)

  }


  if(return.boundary) {
    list(shapefile = shapefile, boundary = clip_boundary)
  } else {
    shapefile
  }

}
