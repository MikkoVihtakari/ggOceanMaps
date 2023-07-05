#' @title Extract depth for coordinates from a raster bathymetry dataset
#' @description Extracts depth from \link{basemap} bathymetry raster dataset for coordinates in a data frame
#' @inheritParams dist2land
#' @param bathy.style Character defining the \link{basemap} bathymetry raster which should be used for the depth extraction. Valid alternatives: \code{"raster_binned"} (or \code{"rb"}), \code{"raster_continuous"} (or \code{"rc"}; default), or \code{"raster_user"} (or \code{"ru"}).
#' @param depth.col The name of the depth column, if \code{bind = TRUE}. Defaults to "depth".
#' @details Uses the \code{\link[stars]{st_extract}} function to extract values from \link{basemap} bathymetry raster grids. Does not work for vector bathymetries.
#' @return Returns a vector if \code{bind = FALSE}, otherwise a data frame. The depths are given in a new column defined by the \code{dist.col} argument. The distances are \strong{kilometers}. \code{NA} distance means that the position is on land. 
#' @author Mikko Vihtakari
#' @examples
#' \dontrun{
#' dt <- data.frame(lon = seq(-20, 80, length.out = 41), lat = 50:90)
#' dt <- get_depth(dt)
#' qmap(dt, color = depth) + scale_color_viridis_c()
#' }
#' @export

# data = data.frame(lon = c(-160, 160, 160, -160), lat = c(80, 80, 60, 60))
# bathy.style = "ru"; lon = NULL; lat = NULL; shapefile = "DecimalDegree"; proj.in = 4326; bind = TRUE; depth.col = "depth"; verbose = TRUE
get_depth <- function(data, bathy.style = "raster_continuous", lon = NULL, lat = NULL, shapefile = "DecimalDegree", proj.in = 4326, bind = TRUE, depth.col = "depth", verbose = FALSE) {
  
  # Bathymetry grid
  
  if(grepl("raster_binned|^rb", bathy.style)) {
    bathy.type <- "raster_binned"
  } else if(grepl("raster_continuous|^rc", bathy.style)) {
    bathy.type <- "raster_continuous"
  } else if(grepl("raster_user|^ru", bathy.style)) {
    bathy.type <- "raster_continuous"
  } else {
    bathy_cmd <- define_bathy_style(bathy.style)
    bathy.type <- gsub("_blues$|_grays$", "", names(bathy_cmd))
    bathy.type <- ifelse(grepl("raster", bathy.type), bathy.type, "vector")
  }
  
  shapefiles <- shapefile_list(shapefile)
  shapefiles$land <- NULL
  shapefiles$glacier <- NULL
  shapefiles$bathy <- shapefiles$bathy[bathy.type]
  
  shapefiles <- load_map_data(shapefiles)
  
  bathy <- shapefiles$bathy$raster
  
  # Data
  
  if(inherits(data, c("sf", "sfc"))) {
    
    ## Extract depths
    
    if(sf::st_crs(bathy) != sf::st_crs(data)) {
      data <- sf::st_transform(data, crs = sf::st_crs(bathy))
    }
    
    out <- stars::st_extract(bathy, data)
    
    ## Return
    
    if(bind) {
      names(out)[names(out) == names(bathy)] <- depth.col
      out
    } else {
      out[[names(bathy)]]
    }
    
  } else {
    
    if(is.null(lon) | is.null(lat)) {
      if(all(!is.data.frame(data))) stop("x argument has to be a data.frame")
      
      tmp <- guess_coordinate_columns(data)
      
      lon <- unname(tmp[names(tmp) == "lon"])
      lat <- unname(tmp[names(tmp) == "lat"])
      
      if(verbose) {
        message(paste0("Used ", lon, " and ", lat, " as input coordinate column names in data"))
      }
      
      if(length(lon) != 1 | length(lat) != 1) {
        stop("lon or lat columns were not found. Define manually.")
      }
    }
    
    ## Remove NA coordinates (and add later)
    
    na.rows <- is.na(data[[lon]]) | is.na(data[[lat]])
    contains.nas <- any(na.rows)
    
    x <- data[!na.rows, c(lon, lat)]
    
    ## Extract depths
    
    x <- sf::st_as_sf(x, coords = tmp, crs = sf::st_crs(proj.in))
    
    if(sf::st_crs(bathy) != sf::st_crs(x)) {
      x <- sf::st_transform(x, crs = sf::st_crs(bathy))
    }
    
    out <- stars::st_extract(bathy, x)
    out <- out[[names(bathy)]]
    
    ## Recombine with NAs
    
    if(contains.nas) {
      na.rows[!na.rows] <- out
      na.rows[na.rows == 1] <- NA
      out <- na.rows
    }
    
    ## Return
    
    if(bind) {
      data[[depth.col]] <- out
      data
    } else {
      out
    }
  }
}