#' @title Quick map
#' @description \code{qmap} is a shortcut similar to ggplot2's \code{\link[ggplot2]{qplot}} designed to quickly plot data with a limited range of options.
#' @param data Data frame to use.
#' @param x,y,... Aesthetics passed into each layer. Longitude and latitude columns are automatically recognized using the \code{\link{guess_coordinate_columns}} function.
#' @param geom Character argument specifying geom(s) to draw. Defaults to "point". Other alternatives are "text" and "label". The "text" option can also be triggered by simply mapping a variable to \code{label} (see Examples).
#' @inheritParams basemap
#' @import ggplot2 ggspatial
#' @return Returns a \link[ggplot2]{ggplot} map, which can be assigned to an object and modified as any ggplot object.
#' @family basemap functions
#' @author Mikko Vihtakari
#' @examples 
#' 
#' dt <- data.frame(lon = c(-100, -80, -60), lat = c(10, 25, 40), var = c("a", "a", "b"))
#' 
#' # Set color
#' 
#' if(requireNamespace("ggOceanMapsData")) {
#' qmap(dt, color = I("red")) 
#' }
#' 
#' # Map color
#' \donttest{
#' qmap(dt, color = var) 
#' }
#' 
#' # Map text
#' \donttest{
#' qmap(dt, label = var) 
#' } 
#' 
#' # All basemap arguments work in qmap()
#' 
#' dt <- data.frame(lon = c(-80, -80, -50, -50), lat = c(65, 80, 80, 65))
#'  
#' \donttest{
#' qmap(dt, rotate = TRUE)
#' }
#' 
#' @export

# data = dt; x = NULL; y = NULL; geom = "point"; limits = NULL; bathymetry = FALSE; glaciers = FALSE; rotate = TRUE; legends = TRUE; legend.position = "right"; lon.interval = NULL; lat.interval = NULL; bathy.style = "poly_blues"; bathy.border.col = NA; bathy.size = 0.1; land.col = "grey60"; land.border.col = "black"; land.size = 0.1; gla.col = "grey95"; gla.border.col = "black"; gla.size = 0.1; grid.col = "grey70"; grid.size = 0.1; base_size = 11; projection.grid = FALSE; verbose = FALSE
qmap <- function(data, x = NULL, y = NULL, geom = "point", limits = NULL, shapefiles = NULL, bathymetry = FALSE, glaciers = FALSE, rotate = FALSE, legends = TRUE, legend.position = "right", lon.interval = NULL, lat.interval = NULL, bathy.style = "poly_blues", bathy.border.col = NA, bathy.size = 0.1, land.col = "grey60", land.border.col = "black", land.size = 0.1, gla.col = "grey95", gla.border.col = "black", gla.size = 0.1, grid.col = "grey70", grid.size = 0.1, base_size = 11, projection.grid = FALSE, expand.factor = 1.1, verbose = FALSE, ...) {
  
  ## Coordinate columns
  
  
  if(!"sf" %in% class(data) & (is.null(x) | is.null(y))) {
    coordCols <- guess_coordinate_columns(data)
    
    if(is.null(x)) {
      x <- unname(coordCols[1])
    }
    
    if(is.null(y)) {
      y <- unname(coordCols[2])
    }
  }
  
  ## Base map
  
  pb <- basemap(
    limits = limits, 
    data = if("sf" %in% class(data)) {data} else {data[c(x, y)]},
    shapefiles = shapefiles,
    bathymetry = bathymetry, glaciers = glaciers, rotate = rotate, 
    legends = legends, legend.position = legend.position, 
    lon.interval = lon.interval, lat.interval = lat.interval, 
    bathy.style = bathy.style, bathy.border.col = bathy.border.col, 
    bathy.size = bathy.size, land.col = land.col, 
    land.border.col = land.border.col, 
    land.size = land.size, gla.col = gla.col, gla.border.col = gla.border.col,
    gla.size = gla.size, grid.col = grid.col, grid.size = grid.size, 
    base_size = base_size, projection.grid = projection.grid, 
    expand.factor = expand.factor, verbose = verbose
  )
  
  ## Geoms
  
  # geom_arguments <- list(
  #   ...,
  #   color = "red",
  #   shape = 21
  # )
  #
  # geom_arguments <- geom_arguments[!duplicated(names(geom_arguments))]
  
  if("sf" %in% class(data)) {
    pb + geom_sf(data = data, aes(...))
  } else if(geom == "point" && !methods::hasArg(label)) {
    pb + ggspatial::geom_spatial_point(data = data, aes(x = get(x), y = get(y), ...), crs = 4326)
  } else if(geom == "text" | methods::hasArg("label")) {
    pb + ggspatial::geom_spatial_text(data = data, aes(x = get(x), y = get(y), ...), crs = 4326)
  } else if(geom == "label") {
    pb + ggspatial::geom_spatial_label(data = data, aes(x = get(x), y = get(y), ...), crs = 4326)
  } else {
    stop("Other geom than point have not been implemented yet.")
  }
  
}
