#' @title Clip a shapefile using a bounding area
#' @description Clips an area from a larger shapefile provided in sf or sp formats.
#' @param x Original shapefile to be clipped as a an \link[sf]{sf} or \code{sp} polygons object. Required. Must contain \code{\link[sf:st_crs]{CRS}} information.
#' @param limits The constraining area used to clip \code{x}. Required. Either a numeric vector of length 4 or a spatial object from the sf or sp packages. The first element of the numeric vector defines the minimum longitude, second element the maximum longitude, third element the minimum latitude and fourth element the maximum latitude of the bounding box. If a spatial object, it must contain \code{\link[sf:st_crs]{CRS}} information. See details.
#' @param proj.limits The \code{\link[sf:st_crs]{CRS}} projection attributes for \code{limits}. Hence format accepted by \code{\link[sf]{st_crs}} will suffice but integers are the easiest. Defaults to decimal degrees.
#' @param simplify Should the \code{x} geometry be simplified before clipping? Useful to make the function faster for large shape files. Uses \code{\link[sf]{st_simplify}} function.
#' @param tol Numerical tolerance value to be used for simplification. See \code{?sf::st_simplfy}.
#' @param return.boundary Logical. If \code{TRUE} returns the clip boundary together with the shapefile.
#' @param extra.validate Logical indicating whether \code{x} should be run through extra validation. Slows down the function but is necessary in some cases involving anti-meridian. 
#' @details The function uses the \code{\link[sf]{st_intersection}} function to clip smaller polygons from larger ones. The clip area is constrained by either a numeric vector or a spatial object in the \code{limits} argument. Defining \code{limits} by a \code{\link[sf:st_sf]{sf}} object gives greater freedom for the clip area as the area does not have to be rectangular.
#' @return Clipped spatial object. If \code{return.boundary = TRUE}, a list containing the shapefile together with the clip boundary.
#' @keywords internal
#' @family create shapefiles
#' @importFrom methods slot slot<-
#' @importFrom grDevices chull
#' @author Mikko Vihtakari
#' @export

# Test parameters
# proj.limits = 4326; simplify = FALSE; tol = 60; return.boundary = FALSE; extra.validate = FALSE
clip_shapefile <- function(x, limits, proj.limits = 4326, simplify = FALSE, tol = 60, return.boundary = FALSE, extra.validate = FALSE) {
  
  ## Checks
  
  if(!inherits(x, c("sf"))) {
    x <- sf::st_as_sf(x)
  }
  
  ## Projection
  
  x_proj <- sf::st_crs(x) 
  
  if(is.na(x_proj)) stop("crs for x is missing. Define the projection attributes and try again.")
  
  ## Clip boundary
  
  if(inherits(limits, c("sf", "sfc", "SpatialPolygonsDataFrame", "SpatialPolygons"))) {
    if(inherits(limits, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
      limits <- sf::st_as_sf(limits)
    }
    
    proj.limits <- sf::st_crs(limits)
    clip_boundary <- limits
    
  } else {
    
    if(!is.numeric(limits)) stop("limits have to be numeric, sf, SpatialPolygonsDataFrame or SpatialPolygons object")
    
    if(length(limits) == 1) {
      
      # stop("Limits with length of 1 has not been implemented properly yet")
      bd <- data.frame(lon = seq(-180, 180, by = 0.5), lat = limits)
      bd <- transform_coord(bd, proj.out = x_proj)
      ch <- grDevices::chull(bd$lat, bd$lon)
      coords <- as.matrix(bd[c(ch, ch[1]), 1:2])
      clip_boundary <- sf::st_sfc(sf::st_polygon(list(coords)), crs = x_proj)
      
    } else if(length(limits) != 4) {
      stop("the length of limits vector has to be 4. See limits argument")
    } else {
      
      coords <- as.matrix(data.frame(
        lon = c(limits[[1]], limits[[2]], limits[[2]], limits[[1]], limits[[1]]),
        lat = c(limits[[3]], limits[[3]], limits[[4]], limits[[4]], limits[[3]])))
      clip_boundary <- sf::st_sfc(sf::st_polygon(list(coords)), crs = sf::st_crs(proj.limits))
      # clip_boundary <- sf::st_segmentize(clip_boundary, units::set_units(0.1, degree))
    }
  }
  
  ### Validate the clip boundary
  
  if(!all(sf::st_is_valid(clip_boundary))) {
    clip_boundary <- sf::st_make_valid(clip_boundary)
  }
  
  ### Validate x
  
  if(extra.validate) {
    if(!all(sf::st_is_valid(x))) {
      x <- sf::st_make_valid(x)
    }
  }
  
  ## Densify (e.g. add vertices) the clip boundary
  
  # clip_boundary <- smoothr::densify(clip_boundary)
  
  ## Check that the projections match
  
  if(sf::st_crs(clip_boundary) != sf::st_crs(x_proj)) {
    clip_boundary <- sf::st_transform(clip_boundary, x_proj)
  }
  
  ## Temporarily turn off s2 while handing decimal degree shapes (anti-meridian as exception, rotated maps. This is some dodgy stuff)
  
  if(suppressWarnings(sf::st_is_longlat(x))) {
    
    tmp <- sf::st_bbox(clip_boundary)[c("xmin", "xmax")]

    if((#sign(tmp[1]) != sign(tmp[2]) &&
        sf::st_crs(x)$proj4string == sf::st_crs(4326)$proj4string)) {
      s2_mode <- sf::sf_use_s2()
      suppressMessages(sf::sf_use_s2(FALSE))
      on.exit({suppressMessages(sf::sf_use_s2(s2_mode))})
    } 
    #else {
    #   if(!all(sf::st_is_valid(x))) {
    #     x <- sf::st_make_valid(x)
    #   }
    # }
  }
  
  ## Simplify bathymetry
  
  if(simplify) {
    x <- sf::st_make_valid(x, oriented = TRUE)
    x <- sf::st_simplify(x, dTolerance = tol)
  }
  
  ## Cropping (st_intersection for round shapes)
  
  if(nrow(sf::st_coordinates(clip_boundary)) > 100) {
    shapefile <- suppressMessages(suppressWarnings(sf::st_intersection(x, clip_boundary)))
  } else {
    shapefile <- suppressWarnings(suppressMessages(sf::st_crop(x, clip_boundary)))
  }
  
  ### Validate shapefile
  
  if(extra.validate) {
    if(!all(sf::st_is_valid(shapefile))) {
      shapefile <- sf::st_make_valid(shapefile)
    }
  }
  
  ## Return
  
  if(return.boundary) {
    list(shapefile = shapefile, boundary = clip_boundary)
  } else {
    shapefile
  }
  
}
