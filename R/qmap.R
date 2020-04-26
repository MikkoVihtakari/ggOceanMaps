#' @title Quick map
#' @description \code{qmap} is a shortcut similar to ggplot2's \code{\link[ggplot2]{qplot}} designed to quickly plot data with a limited range of options.
#' @param data Data frame to use.
#' @param x,y,... Aesthetics passed into each layer. Longitude and latitude columns are automatically recognized using the \code{\link{guess_coordinate_columns}} function.
#' @param geom Character vector specifying geom(s) to draw. Defaults to "point". Other \code{geom}s have not been implemented yet.
#' @param limits Map limits. See the \code{limits} argument in \code{\link{basemap}}. If \code{NULL} the limits are automatically taken from \code{data}
#' @param bathymetry Logical indicating whether bathymetry should be added to the map.
#' @param bathy.style Character defining the style for bathymetry contours. See the \code{bathy.style} argument in \code{\link{basemap}}.
#' @import ggplot2
#' @author Mikko Vihtakari
#' @export

# data = dt; x = NULL; y = NULL; geom = "point"; limits = NULL; bathymetry = FALSE; bathy.style = "poly_blues"
qmap <- function(data, x = NULL, y = NULL, geom = "point", limits = NULL, bathymetry = FALSE, bathy.style = "poly_blues", ...) {

  ## Coordinate columns

  if(is.null(x) | is.null(y)) {
    coordCols <- guess_coordinate_columns(data)

    if(is.null(x)) {
      x <- unname(coordCols[1])
    }

    if(is.null(y)) {
      y <- unname(coordCols[2])
    }
  }

  ## Base map

  pb <- basemap(limits = limits, data = data, bathymetry = bathymetry, bathy.style = bathy.style)

  ## Geoms

  # geom_arguments <- list(
  #   ...,
  #   color = "red",
  #   shape = 21
  # )
  #
  # geom_arguments <- geom_arguments[!duplicated(names(geom_arguments))]


  if(geom == "point") {
    pb + geom_point(data = transform_coord(data), aes(x = get(x), y = get(y)), ...)
  } else {
    stop("Other geom than point have not been implemented yet.")
  }

}
