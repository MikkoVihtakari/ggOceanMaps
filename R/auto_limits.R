#' @title Automatic limits for basemap
#' @description Find limits for a \code{\link{basemap}} from a data frame.
#' @param data Data frame or a spatial object containing data for which the limits should be calculated.
#' @param proj.in Original \code{\link[sf:st_crs]{CRS}} projection. Must be defined as character argument.
#' @param proj.out Resulting map projection. See \code{\link{transform_coord}}.
#' @param lon,lat Names of longitude and latitude columns in \code{data} as character or integer index. If \code{NULL}, the column names are \link[=guess_coordinate_columns]{guessed}.
#' @param expand.factor Expansion factor for map limits. Set to \code{NULL} to ignore.
#' @param verbose Logical indicating whether information about the projection and guessed column names should be returned as message. Set to \code{FALSE} to make the function silent.
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function.
#' @return A list of limits and projections in \code{proj.in} and \code{proj.out} formats.
#' @keywords internal
#' @author Mikko Vihtakari
#' @family customize shapefiles
#' @examples
#' auto_limits(data = expand.grid(lon = c(-120, 180, 120),
#'    lat = c(60, 60, 80)))
#' @export

# lon = NULL; lat = NULL; proj.in = 4326; proj.out = NULL; expand.factor = NULL; verbose = FALSE
auto_limits <- function(data, lon = NULL, lat = NULL, proj.in = 4326, proj.out = NULL, expand.factor = NULL, verbose = FALSE) {
  
  # Get coordinates from spatial objects
  
  if(any(inherits(data, c("sf", "sfc", "SpatialPolygonsDataFrame", "SpatialPolygons")))) {
    tmp <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(data)), proj.in))
    
    data <- expand.grid(data.frame(
      lon = tmp[c("xmin", "xmax")],
      lat = tmp[c("ymin", "ymax")])
    )
  }
  
  # Convert the coordinates
  
  if(is.null(lon) | is.null(lat)) {
    tmp <- guess_coordinate_columns(data)
    
    if(is.null(lon)) {
      lon <- unname(tmp[names(tmp) == "lon"])
    }
    
    if(is.null(lat)) {
      lat <- unname(tmp[names(tmp) == "lat"])
    }
  }
  
  if(verbose) {
    message(paste("Using", lon, "and", lat, "as longitude and latitude columns, respectively."))
  }
  
  # Transform coordinates
  
  x <- transform_coord(x = data, lon = lon, lat = lat, proj.in = proj.in, proj.out = proj.out, verbose = verbose, bind = TRUE)
  
  # if("data.table" %in% class(x)) {
  #   x <- na.omit(x[,c(lon, lat, "lon.proj", "lat.proj"), with = FALSE])
  # } else {
  #   x <- na.omit(x[,c(lon, lat, "lon.proj", "lat.proj")])
  # }
  
  # Coordinate ranges ####
  
  decLims <- c(deg_to_dd(range(dd_to_deg(x[[lon]]), na.rm = TRUE)), range(x[[lat]], na.rm = TRUE))
  
  if(decLims[1] == 180 & sign(decLims[2]) == -1) { # Anti-meridian exception
    decLims[1] <- -180
  }
  
  projLims <- c(range(x[["lon.proj"]], na.rm = TRUE), range(x[["lat.proj"]], na.rm = TRUE))
  
  proj.in <- attributes(x)$proj.in
  proj.out <- attributes(x)$proj.out
  
  if(sf::st_is_longlat(proj.in)) {
    proj.crs <- proj.out
  } else if(sf::st_is_longlat(proj.out)) {
    proj.crs <- proj.in
  } else {
    stop("auto_limits requires either proj.in or proj.out as decimal degrees.")
  }
  
  # Expansion factor
  
  if(!is.null(expand.factor)) {
    
    lon.rdiff <- diff(projLims[1:2])
    lon.shift <- ((lon.rdiff*expand.factor) - lon.rdiff)/2
    projLims[1] <- projLims[1] - lon.shift
    projLims[2] <- projLims[2] + lon.shift
    
    lat.rdiff <- diff(projLims[3:4])
    lat.shift <- ((lat.rdiff*expand.factor) - lat.rdiff)/2
    projLims[3] <- projLims[3] - lat.shift
    projLims[4] <- projLims[4] + lat.shift
    
    # Correct >= 180/90 limits for expanded decimal degree coordinates
    
    if(sf::st_is_longlat(proj.in) & sf::st_is_longlat(proj.out)) {
      if(projLims[1] < -180) projLims[1] <- -180
      if(projLims[2] > 180) projLims[2] <- 180
      if(projLims[3] < -90) projLims[3] <- -90
      if(projLims[3] > 90) projLims[4] <- 90
    }
    
  }
  
  projLims <- stats::setNames(projLims, c("xmin", "xmax", "ymin", "ymax"))
  
  # Projected boundaries
  
  projBound <- sf::st_polygon(
    list(matrix(c(projLims[1], projLims[3], projLims[1], projLims[4], projLims[2], 
                  projLims[4], projLims[2], projLims[3], projLims[1], projLims[3]), 
                ncol = 2, byrow = TRUE)))
  
  projBound <- sf::st_sfc(projBound, crs = sf::st_crs(proj.crs))
  
  tmp <- sf::st_bbox(projBound)
  
  projBoundNodes <- sf::st_multipoint(as.matrix(expand.grid(lon = c(tmp[["xmin"]], tmp[["xmax"]]), lat = c(tmp[["ymin"]], tmp[["ymax"]]))))
  projBoundNodes <- sf::st_sfc(projBoundNodes, crs = sf::st_crs(proj.crs))
  
  # Decimal degree limits
  
  decBoundNodes <- sf::st_transform(projBoundNodes, 4326)
  
  if(!identical(sign(projLims[3]), sign(projLims[4]))) { # Spans across the pole
    decLims <- c(unname(sf::st_bbox(decBoundNodes)[c(1,3,2)]), 90) # old: c(raster::extent(decBoundNodes)[1:3], 90)
    decLims <- c(decLims[1:3], sign(decLims[4]) * 90)
  } else if(sign(decLims[1]) != sign(decLims[2]) & decLims[1] < decLims[2]) { # Antimeridian correction
    decLims <-  unname(sf::st_bbox(decBoundNodes)[c(1,3,2,4)]) # old: c(raster::extent(decBoundNodes)[1:4])
  }
  
  names(decLims) <- c("xmin", "xmax", "ymin", "ymax")
  
  # Return
  
  list(ddLimits = decLims, projLimits = projLims, projBound = projBound, 
       proj.in = attributes(x)$proj.in, proj.out = proj.out)
  
}
