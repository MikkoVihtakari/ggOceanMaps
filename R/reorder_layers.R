#' @title Move \code{basemap} land, glacier and grid layers on top of other ggplot layers
#' @description Moves existing land, glacier and grid layers on top of other layers. Useful for hiding region polygons under land.
#' @param p ggplot object from the \code{\link{basemap}} function.
#' @details This function has not been tested properly yet and is likely to contain bugs. 
#' @return Returns a ggplot object with land, glacier and grid layers on top.
#' @seealso \code{\link{basemap}}
#' @import ggplot2
#' @author Mikko Vihtakari
#' @export
# Test data
# dt <- data.frame(lon = c(seq(-180, 0, 30), seq(30, 180, 30)), lat = -70)
# p <- basemap(limits = -60, glaciers = TRUE, bathymetry = TRUE) #+ geom_spatial_point(data = dt, aes(x = lon, y = lat), color = "red")
# p <- basemap(limits = -60, glaciers = TRUE, bathymetry = TRUE)
# p <- basemap(limits = -60, glaciers = FALSE, bathymetry = TRUE)
# p <- basemap(limits = -60, glaciers = TRUE, bathymetry = FALSE)
# p <- basemap(limits = -60, glaciers = FALSE, bathymetry = FALSE)
#
# p <- basemap(limits = c(0, 60, 68, 82), glaciers = TRUE, bathymetry = TRUE)
# p <- basemap(limits = c(0, 60, 68, 82), glaciers = FALSE, bathymetry = TRUE)
# p <- basemap(limits = c(0, 60, 68, 82), glaciers = TRUE, bathymetry = FALSE)
# p <- basemap(limits = c(0, 60, 68, 82), glaciers = FALSE, bathymetry = FALSE)
# length(p$layers)

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


  # Add coord_sf (which gets lost for some reason)
  
  # Return

  p + coord_sf(expand = FALSE, crs = attributes(p)$crs)
}
