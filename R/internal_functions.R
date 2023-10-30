#' @title Convert font sizes measured as points to ggplot font sizes
#' @description Converts font sizes measured as points (as given by most programs such as MS Word etc.) to ggplot font sizes
#' @param x numeric vector giving the font sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot font sizes
#' @keywords internal
#' @family size adjustors
#' @export
#'
FS <- function(x) x/2.845276 # x is the desired font / line size in pt

#' @title Convert line sizes measured as points to ggplot line sizes
#' @description Converts line sizes measured as points (as given by most programs such as Adobe Illustrator etc.) to ggplot font sizes
#' @param x numeric vector giving the lines sizes in points
#' @return Returns a numeric vector of lenght \code{x} of ggplot line sizes
#' @keywords internal
#' @family size adjustors
#' @export
#'
LS <- function(x) x/2.13

#' @title Select an element of each vector from a list
#' @description Selects y'th element of each vector from a list
#' @param x list
#' @param y number of element. Must be integer
#' @return The selected element from the list
#' @keywords internal
#' @export
#'
select_element <- function(x,y) sapply(x, "[", y)

#' @title Round to multiple of any number
#' @param x numeric vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or \code{\link{round}}
#' @return Rounded numeric vector
#' @keywords internal
#' @author Hadley Wickham
#' @export
#'
round_any <- function(x, accuracy, f = round) {
  f(x / accuracy) * accuracy
}

#' @title Return function output quietly
#' @description Returns function output without printed \code{\link{cat}} messages
#' @param x function
#' @return Output of \code{x}
#' @keywords internal
#' @author Hadley Wickham
#' @export

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

#' @title Convert decimal degree values to angular degrees
#' @description Converts decimal degree values to angular degrees. Used in decimal degree limit calculations.
#' @param x numeric to be converted
#' @return A vector of angular degrees
#' @keywords internal
#' @author Mikko Vihtakari
#' @family degree converters
#' @export

dd_to_deg <- function(x) {
  ifelse(x > 0 & x <= 180, x, ifelse(x <= 0 & x >= -180, 360 + x, NA))
}

#' @title Convert angular degrees to decimal degree values
#' @description Converts angular degree values to decimal degrees. Used in decimal degree limit calculations.
#' @param x numeric to be converted
#' @return A vector of decimal degrees
#' @keywords internal
#' @author Mikko Vihtakari
#' @family degree converters
#' @export

deg_to_dd <- function(x) {
  x <- x - ifelse(x/360 > 1, floor(x/360), 0)*360
  ifelse(x >= 0 & x <= 180, x, ifelse(x > 180, -1*(360 - x), NA))
}

# #' @title Pick a suitable number of cores
# #' @description Picks maximum four cores for parallel processing
# #' @return Integer of suitable number of cores
# #' @keywords internal
# #' @importFrom parallel detectCores
# #' @author The \href{https://github.com/StoXProject/RstoxData/blob/master/R/Utilities.R}{StoXProject}
# #' @export
# 
# getCores <- function() {
#   cores <- as.integer(getOption("mc.cores"))
#   if (length(cores) == 0 || is.na(cores)) {
#     cores <- parallel::detectCores()
#     if (is.na(cores)) {
#       return(1)
#     } else {
#       # Don't use too many cores in autodetect
#       if (cores > 4)
#         return(4)
#       else
#         return(cores)
#     }
#   } else {
#     return(cores)
#   }
# }

#' @title Rotate CRS
#' @description Rotates a CRS such that North is in the middle of two meridians
#' @param crs The CRS to be rotated in \code{\link[sf]{st_crs}} format
#' @param meridians A numeric vector giving the two meridians which should be used in the rotation
#' @return Rotated CRS in \code{\link[sf]{st_crs}} format
#' @keywords internal
#' @author Mikko Vihtakari
#' @export

rotate_crs <- function(crs, meridians) {
  
  ## Mid-longitude
  
  tmp <- dd_to_deg(meridians)
  
  if(tmp[1] > tmp[2]) {
    lonDiff <- 360 - tmp[1] + tmp[2]
  } else {
    lonDiff <- tmp[2] - tmp[1]
  }
  
  midLon <- tmp[1] + lonDiff/2
  midLon <- deg_to_dd(midLon)  
  
  ## CRS rotation
  
  tmp <- unlist(strsplit(sf::st_crs(crs)$proj4string, " "))
  
  if(any(grepl("lon_0=", tmp))) {
    tmp[grepl("lon_0=", tmp)] <- paste0("lon_0=", midLon)
  } else {
    tmp <- c(tmp[-length(tmp)], paste0("lon_0=", midLon), tmp[length(tmp)])
  }
  
  sf::st_crs(paste(tmp, collapse = " "))
  
  
  
}

#' @title Create clip boundary from decimal degree limits
#' @description Creates a clip boundary for map cropping from decimal degree limits 
#' counter-clockwise following parallels
#' @inheritParams auto_limits
#' @param limits Numeric vector of length 4 giving the map limits in decimal degrees. See \link{basemap}.
#' @param crs Coordinate reference system in \code{\link[sf]{st_crs}} format.
#' @keywords internal
#' @author Mikko Vihtakari
#' @export

dd_clip_boundary <- function(limits, crs, expand.factor = NULL) {

  tmp <- dd_to_deg(limits[1:2])

  # if(!is.null(expand.factor)) {
  #   
  #   lon.rdiff <- diff(tmp[1:2])
  #   lon.shift <- ((lon.rdiff*expand.factor) - lon.rdiff)/2
  #   tmp[1] <- tmp[1] - lon.shift
  #   tmp[2] <- tmp[2] + lon.shift
  #   
  #   lat.rdiff <- diff(limits[3:4])
  #   lat.shift <- ((lat.rdiff*expand.factor) - lat.rdiff)/2
  #   limits[3] <- limits[3] - lat.shift
  #   limits[4] <- limits[4] + lat.shift
  #   
  #   # Correct >= 180/90 limits for expanded decimal degree coordinates
  #   
  #   if(any(tmp < 0)) tmp[tmp < 0] <- 0
  #   if(any(tmp > 360)) tmp[tmp > 360] <- 360
  #   if(any(limits[3:4] < -90)) limits[3:4][limits[3:4] < -90] <- -90
  #   if(any(limits[3:4] > 90)) limits[3:4][limits[3:4] > 90] <- 90
  # }
    
  if(tmp[1] > tmp[2]) {
    lonDiff <- 360 - tmp[1] + tmp[2]
  } else {
    lonDiff <- tmp[2] - tmp[1]
  }
  
  midLon <- tmp[1] + lonDiff/2
  
  tmp <- deg_to_dd(c(tmp[1], midLon, tmp[2]))
  
  coords <- data.frame(
    lon = c(tmp, rev(tmp), tmp[1]), 
    lat = c(rep(limits[3], 3), rep(limits[4], 3), limits[3])
  )
  
  tmp <- sf::st_bbox(
    sf::st_transform(
      sf::st_sfc(sf::st_polygon(list(as.matrix(coords))), crs = 4326),
      sf::st_crs(crs)
    )
  )
  
  if(!is.null(expand.factor)) {
    lon.rdiff <- unname(diff(tmp[c("xmin", "xmax")]))
    lon.shift <- ((lon.rdiff*expand.factor) - lon.rdiff)/2
    tmp["xmin"] <- tmp["xmin"] - lon.shift
    tmp["xmax"] <- tmp["xmax"] + lon.shift
    
    lat.rdiff <- unname(diff(tmp[c("ymin", "ymax")]))
    lat.shift <- ((lat.rdiff*expand.factor) - lat.rdiff)/2
    tmp["ymin"] <- tmp["ymin"] - lat.shift
    tmp["ymax"] <- tmp["ymax"] + lat.shift
  }
  
  sf::st_as_sfc(tmp)
}

#' @title Define bathymetry style for \code{\link{basemap}}
#' @description Defines bathymetry style to be used in \code{\link{basemap}}
#' @param x Character argument giving the input bathymetry style. Partially matched and can be abbreviated. See \code{bathy.style} in \code{\link{basemap}}.
#' @return Returns a named character vector with the name pointing to the bathymetry style and value to internal map element command for \code{\link{basemap}} (see \code{\link{map_cmd}}).
#' @keywords internal
#' @author Mikko Vihtakari
#' @export

define_bathy_style <- function(x) {
  
  if(tolower(x) == "rbb") {
    out <- "raster_binned_blues"
  } else if(tolower(x) %in% c("rbg", "raster_binned_greys")) {
    out <- "raster_binned_grays"
  } else if(tolower(x) == "rcb") {
    out <- "raster_continuous_blues"
  } else if(tolower(x) %in% c("rcg", "raster_continuous_greys")) {
    out <- "raster_continuous_grays"
  } else if(tolower(x) %in% c("rub")) {
    out <- "raster_user_blues"
  } else if(tolower(x) %in% c("rug", "raster_user_greys")) {
    out <- "raster_user_grays"
  } else if(tolower(x) %in% c("pb", "pbb", "poly_binned_blues")) {
    out <- "poly_blues"
  } else if(tolower(x) %in% c("pg", "pbg", "poly_greys", "poly_binned_greys", "poly_binned_grays")) {
    out <- "poly_grays"
  } else if(tolower(x) %in% c("cb", "cbb", "contour_binned_blues")) {
    out <- "contour_blues"
  } else if(tolower(x) %in% c("cg", "contour_grey", "contour_greys", "contour_grays")) {
    out <- "contour_gray"
  } else {
    out <- match.arg(
      x,
      c("raster_binned_blues", "raster_binned_grays", "raster_continuous_blues", 
        "raster_continuous_grays", "raster_user_blues", "raster_user_grays",
        "poly_blues", "poly_grays", "contour_blues", "contour_gray")
    )
  }
  
  alternatives <- 
    c("raster_binned_blues" = "bathy_rb", 
      "raster_binned_grays" = "bathy_rb", 
      "raster_continuous_blues" = "bathy_rc",
      "raster_continuous_grays" = "bathy_rc",
      "raster_user_blues" = "bathy_rc",
      "raster_user_grays" = "bathy_rc",
      "poly_blues" = "bathy_pb", 
      "poly_grays" = "bathy_pg", 
      "contour_blues" = "bathy_cb", 
      "contour_gray" = "bathy_cg")
  
  alternatives[names(alternatives) == out]
}
