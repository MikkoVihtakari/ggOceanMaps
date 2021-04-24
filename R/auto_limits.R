#' @title Automatic limits for basemap
#' @description Find limits for a \code{\link{basemap}} from a data frame.
#' @param data Data frame containing data for which the limits should be calculated.
#' @param proj.in Original \code{\link[sp:is.projected]{proj4string}} projection. Must be defined as character argument.
#' @param proj.out Resulting map projection. See \code{\link{transform_coord}}.
#' @param lon,lat Names of longitude and latitude columns in \code{data} as character or integer index. If \code{NULL}, the column names are \link[=guess_coordinate_columns]{guessed}.
#' @param expand.factor Expansion factor for map limits. Set to \code{NULL} to ignore.
#' @param rotate Logical indicating whether the limits should be rotated to point towards the pole relative to mid-longitude limit.
#' @param verbose Logical indicating whether information about the projection and guessed column names should be returned as message. Set to \code{FALSE} to make the function silent.
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. The function does some funky illogical action (seeing the argument names) when feeding in projected coordinates. This is to make the automatic limits to work for projected coordinates too. Despite the funkiness, the function may be useful when customising \code{\link{basemap}}s. 
#' @return A list of limits and projections in \code{proj.in} and \code{proj.out} formats.
#' @keywords internal
#' @author Mikko Vihtakari
#' @import sp
#' @importFrom grDevices chull
#' @family customize shapefiles
#' @examples 
#' auto_limits(data = expand.grid(lon = c(-120, 180, 120), lat = c(60, 60, 80)))
#' @export

# lon = NULL; lat = NULL; proj.in = "+init=epsg:4326"; proj.out = NULL; verbose = FALSE; expand.factor = NULL; verbose = TRUE
auto_limits <- function(data, lon = NULL, lat = NULL, proj.in = "+init=epsg:4326", proj.out = NULL, expand.factor = NULL, verbose = TRUE) {
  
  # Get limits from spatial polygons ####
  
  if(any(class(data) %in% c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
    proj.in <- suppressWarnings(sp::proj4string(data))
    
    if(!grepl("proj=longlat", suppressWarnings(sp::CRS(proj.in)))) {
      data <- sp::spTransform(data, suppressWarnings(sp::CRS("+init=epsg:4326")))
      proj.in <- "+init=epsg:4326"
      message("The data argument is a spatial polygons object, which is not given as decimal degrees. Converted to decimal degrees.")
    }
    
    data <- suppressMessages(ggplot2::fortify(data)[c("long", "lat")])
    names(data) <- c("lon", "lat")
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
  
  x <- transform_coord(x = data, lon = lon, lat = lat, proj.in = proj.in, proj.out = proj.out, verbose = verbose, bind = TRUE)
  
  # if("data.table" %in% class(x)) {
  #   x <- na.omit(x[,c(lon, lat, "lon.proj", "lat.proj"), with = FALSE])
  # } else {
  #   x <- na.omit(x[,c(lon, lat, "lon.proj", "lat.proj")])
  # }
  # 
  # Coordinate ranges ####
  
  if(grepl("proj=longlat", suppressWarnings(sp::CRS(proj.in)))) {
    decLims <- c(deg_to_dd(range(dd_to_deg(x[[lon]]), na.rm = TRUE)), range(x[[lat]], na.rm = TRUE))
    
    if(decLims[1] == 180 & sign(decLims[2]) == -1) { # Anti-meridian exception
      decLims[1] <- -180
    }
    
    projLims <- c(range(x[["lon.proj"]], na.rm = TRUE), range(x[["lat.proj"]], na.rm = TRUE))
    
    proj.in <- attributes(x)$proj.in
    proj.out <- attributes(x)$proj.out
    
  } else if(!grepl("proj=longlat", suppressWarnings(sp::CRS(proj.out)))) {
    projLims <- c(range(x[[lon]], na.rm = TRUE), range(x[[lat]], na.rm = TRUE))
    
    tmp <- suppressWarnings(sp::SpatialPoints(x[c(lon, lat)], proj4string = sp::CRS(proj.out)))
    tmp <- sp::spTransform(tmp, suppressWarnings(sp::CRS(SRS_string = "EPSG:4326")))@bbox  
    decLims <- unname(c(sort(tmp[1,]), sort(tmp[2,])))
    
    proj.in <- attributes(x)$proj.in
    proj.out <- attributes(x)$proj.out
    
  } else {
    decLims <- c(deg_to_dd(range(dd_to_deg(x[["lon.proj"]]), na.rm = TRUE)), range(x[["lat.proj"]], na.rm = TRUE))
    projLims <- c(range(x[[lon]], na.rm = TRUE), range(x[[lat]], na.rm = TRUE))
    
    proj.in <- attributes(x)$proj.out
    proj.out <- attributes(x)$proj.in
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
    
    if(grepl("proj=longlat", suppressWarnings(sp::CRS(proj.in))) & grepl("proj=longlat", suppressWarnings(sp::CRS(proj.out)))) {
      if(projLims[1] < -180) projLims[1] <- -180
      if(projLims[2] > -180) projLims[2] <- 180
      if(projLims[3] < -90) projLims[3] <- -90
      if(projLims[3] > 90) projLims[4] <- 90
    }
    
  }
  
  # Projected boundaries
  
  projBound <- sp::Polygon(matrix(c(projLims[1], projLims[3], projLims[1], projLims[4], projLims[2], projLims[4], projLims[2], projLims[3], projLims[1], projLims[3]), ncol = 2, byrow = TRUE))
  projBound <- sp::SpatialPolygons(list(sp::Polygons(list(projBound), ID = "clip_boundary")), proj4string = suppressWarnings(sp::CRS(proj.out)))
  
  tmp <- as.data.frame(t(sp::bbox(projBound)))
  projBoundNodes <- sp::SpatialPoints(expand.grid(lon = tmp$x, lat = tmp$y), proj4string = suppressWarnings(sp::CRS(proj.out)))
  
  # projxRange <- sp::SpatialPoints(matrix(c(projLims[1], projLims[3], projLims[2], projLims[3]), ncol = 2, byrow = TRUE), proj4string = sp::CRS(proj.out))
  # projyRange <- sp::SpatialPoints(matrix(c(projLims[1], projLims[3], projLims[1], projLims[4]), ncol = 2, byrow = TRUE), proj4string = sp::CRS(proj.out))
  # 
  # Decimal degree limits
  
  # decBound <- sp::spTransform(projBound, sp::CRS(attributes(x)$proj.in))
  
  # decxRange <- sp::spTransform(projxRange, sp::CRS(proj.in))
  # decyRange <- sp::spTransform(projyRange, sp::CRS(proj.in))
  
  decBoundNodes <- sp::spTransform(projBoundNodes, suppressWarnings(sp::CRS(SRS_string = "EPSG:4326"))) # proj.in
  
  if(!identical(sign(projLims[3]), sign(projLims[4]))) { # Spans across the pole
    decLims <- c(raster::extent(decBoundNodes)[1:3], 90)
  } else if(sign(decLims[1]) != sign(decLims[2]) & decLims[1] < decLims[2]) { # Antimeridian correction
    decLims <- c(raster::extent(decBoundNodes)[1:4])
  } 
  
  # Return
  
  list(ddLimits = decLims, projLimits = projLims, projBound = projBound, proj.in = attributes(x)$proj.in, proj.out = proj.out)
  
}
