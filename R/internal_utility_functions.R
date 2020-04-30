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
#' @keywords internal
#' @export
#'
select_element <- function(x,y) sapply(x, "[", y)

#' @title Round to multiple of any number
#' @param x numeric vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: \code{\link{floor}}, \code{\link{ceiling}} or
#'  \code{\link{round}}
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
#' @keywords internal
#' @author Mikko Vihtakari
#' @family degree converters
#' @export

deg_to_dd <- function(x) {
  x <- x - ifelse(x/360 > 1, floor(x/360), 0)*360
  ifelse(x > 0 & x <= 180, x, ifelse(x > 180, -1*(360 - x), NA))
}


# Define global variables
utils::globalVariables(c("rdiff.lon", "rdiff.lat"))

# "long", "lat", "group", "round.lat", "round.lon", "n.lat.grid", "n.lon.grid", "lat.interval", "lon.interval", "keep.glaciers", "MapType", "proj4.utm", "n.points", "..level..", "abb", "x", "y", "sal", "temp", "xmin", "xmax", "ymin", "ymax", ".", "variable", "ymin", "ymax", "X", "arctic_bathy", "barents_bathy", "barents_currents", "kongsfjord_watermasses", "svalbard_bathy"
