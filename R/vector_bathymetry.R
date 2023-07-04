#' @title Create a polygon bathymetry from a raster bathymetry file
#' @description Vectorizes bathymetry rasters. Designed to be used for the output of \code{\link{raster_bathymetry}} function. Warning: processing may take a long time if the bathymetry raster is large.
#' @param bathy bathyRaster object from the \code{\link{raster_bathymetry}} function.
#' @param drop.crumbs Single numeric value specifying a threshold (area in km2) for disconnected polygons which should be removed. Set to \code{NULL} to bypass the removal. Uses the \link[smoothr]{drop_crumbs} function.
#' @param remove.holes Single numeric value specifying a threshold (area in km2) for holes which should be removed. Set to \code{NULL} to bypass the removal. Uses the \link[smoothr]{fill_holes} function. Currently VERY slow. 
#' @param smooth Logical indicating whether the pixelated contours should be smoothed. Uses the \link[smoothr]{smooth_ksmooth} function.
#' @details The \code{drop.crumbs} and \code{remove.holes} arguments can be used to make the resulting object smaller in file size. The \code{smooth} argument can be used to remove the pixelated contours, but often increases file size. Note also that using this option will bias the contours with respect to real world.
#' @return An \link[sf:st_polygon]{sf} object containing the depth polygons. Uses same projection than \code{bathy} (see \code{\link[sf:st_crs]{CRS}}).
#' @author Mikko Vihtakari
#' @family create shapefiles
#' @export


# bathy = rb; drop.crumbs = NULL; remove.holes = NULL; smooth = FALSE
vector_bathymetry <- function(bathy, drop.crumbs = NULL, remove.holes = NULL, smooth = FALSE) {
  
  ## Turn off s2
  # s2_mode <- sf::sf_use_s2()
  # suppressMessages(sf::sf_use_s2(FALSE))
  # on.exit({suppressMessages(sf::sf_use_s2(s2_mode))})
  
  # Progress bar ####
  
  pb <- utils::txtProgressBar(min = 0, max = 7, initial = 0, style = 3)
  
  ## General checks ####
  
  ### Bathy argument
  
  if(!inherits(bathy, "bathyRaster")) stop("bathy has to be output from the raster_bathymetry function.")
  if(is.na(sf::st_crs(bathy$raster))) stop("bathy does not contain coordinate reference information")
  if(!inherits(bathy$raster, c("stars", "stars_proxy"))) stop("bathy$raster has to be a stars object")
  
  ### The drop.crumbs argument
  
  if(!is.null(drop.crumbs)) {
    if(!(is.vector(drop.crumbs) & class(drop.crumbs) %in% c("numeric", "integer") & length(drop.crumbs) == 1)) {
      stop("The drop.crumbs parameter has to be a single value.")
    }
  }
  
  utils::setTxtProgressBar(pb, 1)
  
  ## Polygonization ####
  
  pol <- sf::st_as_sf(bathy$raster, as_points = FALSE, merge = TRUE)
  
  utils::setTxtProgressBar(pb, 2)
  
  ### Validate the polygon
  
  # if(!all(sf::st_is_valid(pol))) {
  pol <- sf::st_make_valid(pol) 
  
  if(!all(sf::st_is_valid(pol))) stop("The initial geometry validation did not work. You are skrewed...or try the buffer +/- trick.")
  # }
  
  utils::setTxtProgressBar(pb, 3)
  
  ## Drop crumbs and holes
  
  if(!is.null(drop.crumbs)) {
    pol <- pol[sf::st_area(pol) > units::set_units(drop.crumbs, "km^2", mode = "standard"),]
  }
  
  utils::setTxtProgressBar(pb, 4)
  
  if(!is.null(remove.holes)) {
    # This operation is painfully slow. nngeo::st_remove_holes(pol, max_area = remove.holes * 1e6) would
    # potentially do the same, but is also slow. 
    pol <- smoothr::fill_holes(pol, units::set_units(remove.holes, "km^2", mode = "standard"))
  }
  
  utils::setTxtProgressBar(pb, 5)
  
  ## Smooth and simplify
  
  if(smooth) {
    
    pol <- smoothr::smooth(pol, method = "ksmooth")
    
    if(!all(sf::st_is_valid(pol))) {
      pol <- sf::st_make_valid(pol)
    }
    
    pol <- sf::st_simplify(pol, preserveTopology = TRUE)
    
  }
  
  utils::setTxtProgressBar(pb, 6)
  
  ## Manipulate depth data
  
  names(pol)[1] <- "depth"
  
  # pol$depth <- factor(pol$depth, levels = rev(levels(pol$depth)))
  # tmp <- pol$depth
  # 
  # tmp <- factor(tmp, levels = sort(unique(tmp)))
  # 
  # level_key <- bathy$depth.invervals$interval[c(-nrow(bathy$depth.invervals))]
  # names(level_key) <- bathy$depth.invervals$average[c(-nrow(bathy$depth.invervals))]
  # level_key <- rev(level_key)
  # 
  # tmp <- dplyr::recode_factor(tmp, !!!level_key)
  # 
  # pol$depth <- tmp
  
  ## Final validation
  
  if(!all(sf::st_is_valid(pol))) {
    pol <- sf::st_make_valid(pol)
  }
  
  if(!all(sf::st_is_valid(pol))) stop("The final geometry validation did not work. Adjust something.")
  
  utils::setTxtProgressBar(pb, 7)
  
  ## Return
  
  return(pol)
  
}
