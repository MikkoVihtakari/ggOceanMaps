#' @title Move \code{basemap} land, glacier and grid layers on top of other ggplot layers
#' @description Moves existing land, glacier and grid layers on top of other layers. Useful for hiding region polygons under land.
#' @param p ggplot object from the \code{\link{basemap}} function.
#' @details This function has not been tested properly yet and is likely to contain bugs. 
#' @return Returns a ggplot object with land, glacier and grid layers on top.
#' @family customize shapefiles
#' @import ggplot2
#' @author Mikko Vihtakari
#' @examples
#' if(requireNamespace("ggOceanMapsData", quietly = TRUE)) {
#'  \donttest{
#'  data("ices_areas")
#'  p <- basemap(c(-20, 15, 50, 70)) + 
#'    annotation_spatial(ices_areas, aes(fill = Area_Full), show.legend = FALSE)
#'  
#'  # Polygons on top of land
#'  p
#'  
#'  # Move land on top
#'  reorder_layers(p)
#'  }
#'  }
#' @export

reorder_layers <- function(p) {

  if(!"ggplot" %in% class(p)) stop("p has to be a ggplot object")

  # Bathymetry & glaciers?

  tmp <- attributes(p)
  polarMap <- tmp$polarmap 
  
  if(tmp$bathymetry & tmp$glaciers) {
    if(polarMap) {
      if(length(p$layers) > 6) p$layers <- c(p$layers[c(1, 7:length(p$layers))], p$layers[2:6])
    } else {
      if(length(p$layers) > 3) p$layers <- c(p$layers[c(1, 4:length(p$layers))], p$layers[2:3])
    }
  } else if(tmp$bathymetry) {
    if(polarMap) {
      if(length(p$layers) > 5) p$layers <- c(p$layers[c(1, 6:length(p$layers))], p$layers[2:5])
    } else {
      if(length(p$layers) > 2) p$layers <- c(p$layers[c(1, 3:length(p$layers))], p$layers[2])
    }
  } else if(tmp$glaciers) {
    if(polarMap) {
      if(length(p$layers) > 5) p$layers <- c(p$layers[6:length(p$layers)], p$layers[1:5])
    } else {
      if(length(p$layers) > 2) p$layers <- c(p$layers[3:length(p$layers)], p$layers[1:2])
    }
  } else { # Land only
    if(polarMap) {
      if(length(p$layers) > 4) p$layers <- c(p$layers[5:length(p$layers)], p$layers[1:4])
    } else {
      if(length(p$layers) > 1) p$layers <- c(p$layers[2:length(p$layers)], p$layers[1])
    }
  } 


  # Return, (add coord_sf, which gets lost for some reason)

  p + coord_sf(expand = FALSE, crs = attributes(p)$crs, xlim = attributes(p)$limits[1:2], ylim = attributes(p)$limits[3:4])
}
