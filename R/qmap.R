#' @title Quick map
#' @description \code{qmap} is a shortcut similar to ggplot2's \code{\link[ggplot2]{qplot}} designed to quickly plot data with a limited range of options.
#' @param data Data frame to use.
#' @param x,y,... Aesthetics passed into each layer. Longitude and latitude columns are automatically recognized using the \code{\link{guess_coordinate_columns}} function.
#' @param geom Character vector specifying geom(s) to draw. Defaults to "point". Other \code{geom}s have not been implemented yet.
#' @param limits Map limits. See the \code{limits} argument in \code{\link{basemap}}. If \code{NULL} the limits are automatically taken from \code{data}
#' @param resolution Not implemented yet.
#' @param bathymetry Logical indicating whether bathymetry should be added to the map.
#' @param glaciers Logical indicating whether glaciers and ice-sheets should be added to the map.
#' @param rotate Logical indicating whether the projected maps should be rotated to point towards the pole relative to mid-longitude limit. Experimental.
#' @param bathy.style Character defining the style for bathymetry contours. Alternatives:
#' \itemize{
#' \item \code{"poly_blues"} plots polygons filled with different shades of blue.
#' \item \code{"poly_greys"} plots polygons filled with different shades of gray.
#' \item \code{"contour_blues"} contour lines with different shades of blue.
#' \item \code{"contour_grey"} plots gray contour lines.
#' }
#' @param legends Logical indicating whether the legend for bathymetry should be shown.
#' @param legend.position The position for ggplot2 legend. See the argument with the same name in \link[ggplot2]{theme}.
#' @param lon.interval,lat.interval Numeric value specifying the interval of longitude and latitude grids. \code{NULL} finds reasonable defaults depending on \code{limits}.
#' @param land.col,gla.col,grid.col Character code specifying the color of land, glaciers and grid lines, respectively. Use \code{NA} to remove the grid lines.
#' @param land.border.col,gla.border.col,bathy.border.col Character code specifying the color of the border line for land, glacier, and bathymetry shapes.
#' @param land.size,gla.size,bathy.size,grid.size Numeric value specifying the width of the border line land, glacier and bathymetry shapes as well as the grid lines, respectively. Use the \code{\link{LS}} function for a specific width in pt. See Details.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{theme_bw}.
#' @param projection.grid Logical indicating whether the coordinate grid should show projected coordinates instead of decimal degree values. Useful to define limits for large maps in polar regions.
#' @param verbose Logical indicating whether information about the projection and guessed column names should be returned as message. Set to \code{FALSE} to make the function silent.
#' @import ggplot2
#' @family basemap functions
#' @author Mikko Vihtakari
#' @examples 
#' 
#' dt <- data.frame(lon = c(-100, -80, -60), lat = c(10, 25, 40))
#' qmap(dt, color = "red")
#' 
#' @export

# data = dt; x = NULL; y = NULL; geom = "point"; limits = NULL; bathymetry = FALSE; bathy.style = "poly_blues"
qmap <- function(data, x = NULL, y = NULL, geom = "point", limits = NULL, bathymetry = FALSE, glaciers = FALSE, resolution = "low", rotate = FALSE, legends = TRUE, legend.position = "right", lon.interval = NULL, lat.interval = NULL, bathy.style = "poly_blues", bathy.border.col = NA, bathy.size = 0.1, land.col = "grey60", land.border.col = "black", land.size = 0.1, gla.col = "grey95", gla.border.col = "black", gla.size = 0.1, grid.col = "grey70", grid.size = 0.1, base_size = 11, projection.grid = FALSE, verbose = TRUE, ...) {

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

  pb <- basemap(limits = limits, data = data, bathymetry = bathymetry, bathy.style = bathy.style, land.col = land.col)

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
