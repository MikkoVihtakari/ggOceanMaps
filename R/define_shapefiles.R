#' @title Define a shapefile to use in plotting from the limits argument
#' @description An internal function to make \code{\link{basemap}} code more readable
#' @param limits A numeric vector of length 4: The first element defines the minimum longitude, the second element the maximum longitude, the third element the minimum latitude and the fourth element the maximum latitude of the bounding box.
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function.
#' @return A list containing the correct shapefile and a logical statement whether the limits were supplied as decimal degrees. 
#' @keywords internal
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}
#' @export

define_shapefiles <- function(limits) {

  if(length(limits) == 1) {

    if(limits >= 30 & limits <= 89) {
      shapefiles <- "ArcticStereographic"
      decLimits <- TRUE
    } else if (limits <= -30 & limits >= -89) {
      shapefiles <- "AntarcticStereographic"
      decLimits <- TRUE
    } else {
      stop("limits argument has to be between 30 and 89 or -30 and -89 when numeric and length == 1 (polar stereographic maps).")
    }
  } else if(length(limits) == 4) {

    if(is_decimal_limit(limits)) {
      decLimits <- TRUE

      
      if(limits[4] > 70) {
        shapefiles <- "ArcticStereographic"
      } else if(limits[4] < -70) {
        shapefiles <- "AntarcticStereographic"
      } else if(min(abs(limits[3:4])) < 30) {
        shapefiles <- "DecimalDegree"
      } else if(max(abs(limits[3:4])) < 60) {
        shapefiles <- "DecimalDegree"
      } else if(max(limits[3:4]) >= 60) {
        shapefiles <- "ArcticStereographic"
      } else if(min(limits[3:4]) <= -60) {
        shapefiles <- "AntarcticStereographic"
      } else {
        stop("Unexpected error in decimal degree shapefile definition")
      }

    } else {
      decLimits <- FALSE
      shapefiles <- NA
    }

  }

  list(shapefile.name = shapefiles, decimal.degree.limits = decLimits)
}
