#' @title Calculate distance to the closest land for coordinates
#' @description Calculates the closest distance to land for coordinates in a data frame
#' @param data Data frame or \link[sf:sf]{sf} object containing geographic coordinates.
#' @param lon,lat Either the names of the longitude and latitude columns in \code{data} or \code{NULL} to \link[=guess_coordinate_columns]{guess the longitude and/or latitude columns} in \code{data}.
#' @param proj.in \code{\link[sf:st_crs]{coordinate reference system}} of \code{data}.
#' @param shapefile Land shape to which distances should be calculated. Either a character argument referring to a name of pre-made shapefiles in \code{\link{shapefile_list}}, a single \link[sf]{sf} or \code{sp} polygons object object or \code{NULL} to enable automatic definition of the land shapes based on \code{data}. Set to \code{"DecimalDegree"} by default which enables great circle distances using \link[sf]{s2} features assuming a spherical Earth (as a contrast to earlier versions of the function which used flat Earth).
#' @param bind Logical indicating whether \code{x} should be returned with the distances (\code{TRUE}, default) or should the distances be returned as vector (\code{FALSE}).
#' @param dist.col The name of the distance column, if \code{bind = TRUE}. Defaults to "ldist".
#' @param binary Logical indicating whether binary (TRUE = the position is in the ocean, FALSE = the position is on land) should be returned instead of distances. Speeds up the function considerably.
#' @param verbose Logical indicating whether information about the process should be returned as messages. Set to \code{FALSE} to make the function silent.
#' @details The function calculates great circle spherical distances using the \code{\link[sf]{st_distance}} function by default. The function can be slow for large datasets. If you only want to use the function to remove (wrong) observations reported on land, set the \code{binary} argument to \code{TRUE}. This speeds up the calculations by a factor of ten.
#' @return Returns a vector if \code{bind = FALSE}, otherwise a data frame. The distances are given in a new column defined by the \code{dist.col} argument. The distances are \strong{kilometers} if \code{binary = FALSE}, otherwise logical (TRUE = the position is in the ocean, FALSE = the position is on land).
#' @author Mikko Vihtakari
#' @examples
#' # Simple example:
#' dt <- data.frame(lon = seq(-20, 80, length.out = 41), lat = 50:90)
#' dt <- dist2land(dt, verbose = FALSE)
#' \donttest{
#' qmap(dt, color = ldist) + scale_color_viridis_c()
#' 
#' # Datasets covering the entire Earth seem to work now, except 0,0 lon/lat point
#' lon = deg_to_dd(seq(0,360,30)); lat = c(80,50,20,0,-20,-50,-80)
#'
#' dt <- data.frame(
#'  lon = rep(lon, length(lat)), lat = rep(lat, each = length(lon)))
#'
#' qmap(dist2land(dt, verbose = FALSE), color = ldist) +
#'  scale_color_viridis_c()
#' }
#' \dontrun{
#' dt <- data.frame(
#'   lon = deg_to_dd(seq(0,360,length.out = 1e3)), 
#'   lat = rep(60, 1000))
#'   
#' # The distance calculation is slow for large datasets
#' system.time(dist2land(dt))
#' # user  system elapsed 
#' # 12.677   0.146  12.849 
#'
#' # binary = TRUE speeds the function up
#' system.time(dist2land(dt, binary = TRUE))
#' # user  system elapsed 
#' # 1.239   0.120   1.369 
#' }
#' @export

## Test parameters
# lon = NULL; lat = NULL; shapefile = "DecimalDegree"; proj.in = 4326; bind = TRUE; dist.col = "ldist"; binary = FALSE; verbose = TRUE

dist2land <- function(data, lon = NULL, lat = NULL, shapefile = "DecimalDegree", proj.in = 4326, bind = TRUE, dist.col = "ldist", binary = FALSE, verbose = TRUE) {
  
  # sf data
  
  if(inherits(data, c("sf", "sfc"))) { 
    
    x <- data
    
  } else {
    ## Case for defined x and undefined lon or/and lat ####
    
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
    
    ## Data
    
    ### Remove NA coordinates (and add later)
    
    na.rows <- is.na(data[[lon]]) | is.na(data[[lat]])
    contains.nas <- any(na.rows)
    
    x <- as.data.frame(data[!na.rows, c(lon, lat)])
    colnames(x) <- c("lon", "lat")
    
    x <- sf::st_as_sf(x, coords = c("lon", "lat"), crs = sf::st_crs(proj.in))
    
  }
    
    ## Land shape ###
    
    if(!is.null(shapefile)) {
      
      error_test <- quiet(try(match.arg(shapefile, shapefile_list("all")$name), silent = TRUE))
      
      if(!inherits(error_test, "try-error")) {
        shapefile <- shapefile_list(shapefile)
        if(verbose) message(paste("Using", shapefile$name, "as land shapes."))
        land <- eval(parse(text = shapefile$land))
      } else {
        if(!inherits(shapefile, c("sf", "sfc", "SpatialPolygonsDataFrame", "SpatialPolygons"))) stop("The shapefile must either be matchable string to shapefile_list or a sf or sp polygons object.")
        if(verbose) message("Using custom land shapes.")
        if(inherits(shapefile, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
          land <- sf::st_as_sf(shapefile)
        } else {
          land <- shapefile
        }
      }
    } else {
      
      ddLimits <- auto_limits(data = x, lon = "lon", lat = "lat", proj.in = proj.in, verbose = FALSE)$ddLimits
      
      shapefile.def <- define_shapefiles(ddLimits)
      shapefile <- shapefile_list(shapefile.def$shapefile.name)
      if(verbose) message(paste("Using", shapefile$name, "as land shapes."))
      
      land <- eval(parse(text = shapefile$land))
    }
    
    ## Coordinate points ###
    
  if(sf::st_crs(land) != sf::st_crs(x)) {
    x <- sf::st_transform(x, sf::st_crs(land))
  }
  
  ############################
  ## Distance calculation ####
  
  if(binary) { ## Binary positions
    
    if(verbose) message("Calculating binary positions...")
    
    tmp <- sapply(suppressMessages(sf::st_intersects(x, land)), function(k) length(k) == 0)
    if(verbose) message("Returning binary positions: TRUE in the ocean, FALSE on land.")
    
  } else { ## Distance
    
    if(verbose) message("Calculating distances...")
    tmp <- apply(sf::st_distance(x, land), 1, min)/1e3 # km
    
    if(sf::st_is_longlat(x)) {
      if(verbose) message("Returning great circle spherical distances from land as kilometers.")
    } else {
      if(verbose) message("Returning Euclidean distances from land as kilometers.")
      
    }
  }
  
  ## Splitting x to number of available cores and running the distance calculus parallel would be possible but not implemented due to focus on other tasks in 2.0
  ## Saving the parallelization code from 1.3.7 ggOceanMaps version so that it can be implemented later when there's time.
  # cores <- min(length(x), cores)
  # 
  # if(verbose) message("Calculating distances with parallel processing...")
  # 
  # # On Windows run special args to speed up:
  # if(.Platform$OS.type == "windows") {
  #   cl <- parallel::makeCluster(cores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
  #   out <- parallel::parLapply(cl, 1:length(x), function(i) suppressWarnings(gDistance(x[i], land)/1000))
  #   parallel::stopCluster(cl)
  #   tmp <- unlist(out)
  # }
  # else {
  #   tmp <- unlist(parallel::mclapply(1:length(x), function(i) {
  #     suppressWarnings(gDistance(x[i], land)/1000)
  #   }, mc.cores = cores))
  # }
  
  ## Return
  
  if(!inherits(data, c("sf", "sfc"))) { 
    
    if(contains.nas) {
      na.rows[!na.rows] <- tmp
      na.rows[na.rows == 1] <- NA
      tmp <- na.rows
    }
  }
  
  if(bind) {
    data[[dist.col]] <- tmp
    data
  } else {
    tmp
  }
}


