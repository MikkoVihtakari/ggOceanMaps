#' @title Automatic limits for basemap
#' @description Find limits for a \code{\link{basemap}} from a data frame.
#' @param data Data frame containing data for which the limits should be calculated.
#' @param proj.in Original \code{\link[sp]{proj4string}} projection. Must be defined as character argument.
#' @param proj.out Resulting map projection. See \code{\link{transform_coord}}.
#' @param lon,lat Names of longitude and latitude columns in \code{data} as character or integer index. If \code{NULL}, the column names are \link[=guess_coordinate_columns]{guessed}.
#' @param expand.factor Expansion factor for map limits. Set to \code{NULL} to ignore.
#' @param rotate Logical indicating whether the limits should be rotated to point towards the pole relative to mid-longitude limit.
#' @param verbose if \code{TRUE}, the function prints information about the changed projection. Switch to \code{FALSE} to make the function silent.
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. The function does some funky illogical action (seeing the argument names) when feeding in projected coordinates. This is to make the automatic limits to work for projected coordinates too. Despite the funkiness, the function may be useful when customising \code{\link{basemap}}s. 
#' @keywords internal
#' @author Mikko Vihtakari
#' @import sp
#' @importFrom grDevices chull
#' @family basemap functions
#' @export

# lon = NULL; lat = NULL; proj.in = "+init=epsg:4326"; proj.out = NULL; verbose = FALSE; expand.factor = NULL; rotate = TRUE
auto_limits <- function(data, lon = NULL, lat = NULL, proj.in = "+init=epsg:4326", proj.out = NULL, expand.factor = NULL, verbose = FALSE) {

  # Convert the coordinates

  x <- transform_coord(x = data, lon = lon, lat = lat, proj.in = proj.in, proj.out = proj.out, verbose = verbose, bind = TRUE)

  # Coordinate ranges

  if(proj.in == "+init=epsg:4326") {
    decLims <- c(deg_to_dd(range(dd_to_deg(x[[1]]), na.rm = TRUE)), range(x[[2]], na.rm = TRUE))
    projLims <- c(range(x[[3]], na.rm = TRUE), range(x[[4]], na.rm = TRUE))
    proj.in <- attributes(x)$proj.in
    proj.out <- attributes(x)$proj.out
  } else  {
    decLims <- c(deg_to_dd(range(dd_to_deg(x[[3]]), na.rm = TRUE)), range(x[[4]], na.rm = TRUE))
    projLims <- c(range(x[[1]], na.rm = TRUE), range(x[[2]], na.rm = TRUE))
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
  }

  # Projected boundaries

  projBound <- sp::Polygon(matrix(c(projLims[1], projLims[3], projLims[1], projLims[4], projLims[2], projLims[4], projLims[2], projLims[3], projLims[1], projLims[3]), ncol = 2, byrow = TRUE))
  projBound <- sp::SpatialPolygons(list(sp::Polygons(list(projBound), ID = "clip_boundary")), proj4string = sp::CRS(proj.out))

  # if(rotate) {
  #
  #   # Add the mid-longitude
  #
  #   if(grepl("proj=longlat", sp::CRS(proj.in))) {
  #     tmp <- dd_to_deg(decLims[1:2])
  #
  #     if(tmp[1] > tmp[2]) {
  #       lonDiff <- 360 - tmp[1] + tmp[2]
  #     } else {
  #       lonDiff <- tmp[2] - tmp[1]
  #     }
  #
  #     midLon <- tmp[1] + lonDiff/2
  #     midLon <- deg_to_dd(midLon)
  #
  #   }
  #
  #
  #   proj.out <- sp::CRS(attributes(x)$proj.out)
  #   proj.out <- gsub("lon_0=0", paste0("lon_0=", midLon), proj.out)
  #
  #   projBoundRot <- sp::spTransform(projBound, CRSobj = sp::CRS(proj.out))
  #
  #   projLims <- raster::extent(projBoundRot)[1:4]
  #   projxRange <- sp::SpatialPoints(matrix(c(projLims[1], projLims[3], projLims[2], projLims[3]), ncol = 2, byrow = TRUE), proj4string = sp::CRS(proj.out))
  #   projyRange <- sp::SpatialPoints(matrix(c(projLims[1], projLims[3], projLims[1], projLims[4]), ncol = 2, byrow = TRUE), proj4string = sp::CRS(proj.out))
  #
  # } else {

    projxRange <- sp::SpatialPoints(matrix(c(projLims[1], projLims[3], projLims[2], projLims[3]), ncol = 2, byrow = TRUE), proj4string = sp::CRS(proj.out))
    projyRange <- sp::SpatialPoints(matrix(c(projLims[1], projLims[3], projLims[1], projLims[4]), ncol = 2, byrow = TRUE), proj4string = sp::CRS(proj.out))

  #   projBoundRot <- NULL
  #
  # }

  # Decimal degree limits

  # decBound <- sp::spTransform(projBound, sp::CRS(attributes(x)$proj.in))

  decxRange <- sp::spTransform(projxRange, sp::CRS(proj.in))
  decyRange <- sp::spTransform(projyRange, sp::CRS(proj.in))

  if(!identical(sign(projLims[3]), sign(projLims[4]))) { # Spans across the pole
    decLims <- c(raster::extent(decxRange)[1:2], raster::extent(decyRange)[3], 90)
  } else {
    decLims <- c(raster::extent(decxRange)[1:2], raster::extent(decyRange)[3:4])
  }

  # Return

  list(ddLimits = decLims, projLimits = projLims, projBound = projBound, proj.in = attributes(x)$proj.in, proj.out = proj.out)

}
