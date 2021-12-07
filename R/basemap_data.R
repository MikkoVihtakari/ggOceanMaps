#' @title Create basemapData object for basemap plotting
#' @description Internal function to create a \code{basemapData} object for \code{\link{basemap}}
#' @inheritParams basemap
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. Common users do not need to worry about these details.
#' @return A list of class \code{basemapData} containing information required for plotting a \code{\link{basemap}}.
#' @keywords internal
#' @export
#' @import sp sf raster rgeos
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}

# Test paramters
# limits = NULL; data = NULL; shapefiles = NULL; bathymetry = FALSE; glaciers = FALSE; lon.interval = NULL; lat.interval = NULL; expand.factor = 1.1; rotate = FALSE; verbose = TRUE
basemap_data <- function(limits = NULL, data = NULL, shapefiles = NULL, bathymetry = FALSE, glaciers = FALSE, lon.interval = NULL, lat.interval = NULL, expand.factor = 1.1, rotate = FALSE, verbose = FALSE) {
  
  # Switches and checks ####
  
  shapefilesDefined <- FALSE
  polarMap <- FALSE
  
  # 1. shapefiles argument dictates the used shapefile. If NULL, shapefiles are obtained from limits ###
  
  if(!is.null(shapefiles)) {
    
    error_test <- quiet(try(match.arg(shapefiles, shapefile_list("all")$name), silent = TRUE))
    
    if(class(error_test) != "try-error") {
      shapefiles <- shapefile_list(shapefiles)
      
      ## Load the shapefiles if download required
      
      if(!is.na(shapefiles$path)) {
        tmp <- load_map_data(x = shapefiles)
        
        shapefiles <- lapply(shapefiles, function(k) {
          test <- which(names(tmp) == k)
          
          if(length(test) != 1) {
            k
          } else {
            tmp[[test]]
          }
        })
        
        shapefilesDefined <- TRUE
      }
      
    } else {
      if(any(!c("land", "glacier", "bathy") %in% names(shapefiles))) stop("Shapefiles object must be a list and contain named elements: 'land', 'glacier', 'bathy'. See Details.")
      customShapefiles <- sapply(shapefiles[c("land", "glacier", "bathy")], function(k) class(k))
      
      if(any(!customShapefiles %in% c("NULL", "SpatialPolygonsDataFrame", "SpatialPolygons"))) stop("Shapefiles elements 'land', 'glacier', and 'bathy' must either be a SpatialPolygonsDataFrame, SpatialPolygons, or NULL. See Details.")
      if(all(customShapefiles == "NULL")) stop("One of following shapefiles elements 'land', 'glacier', and 'bathy' must be a SpatialPolygonsDataFrame. See Details.")
      
      shapefilesDefined <- TRUE
    }
  }
  
  # 2. limits argument dictates the limits and defines shapefile if not specified ###
  
  if(!is.null(limits)) {
    
    # Checks
    
    if(!is.numeric(limits)) stop("Limits have to be given as numeric vectors of length 1 or 4. See Details.")
    if(!length(limits) %in% c(1, 4)) stop("Limits has to be either length of 1 or 4. See Details.")
    
    decLimits <- is_decimal_limit(limits)
    
    # Polar maps
    
    if(length(limits) == 1) {
      if(!decLimits) stop("Limits of length 1 have to be given as decimal degrees.")
      
      if(abs(limits) <= 89 & abs(limits) >= 30) {
        polarMap <- TRUE
      } else {
        stop("The limits argument has to be between 30 and 89 (or negative) for polar stereographic maps.")
      }
      
    } else if(
      (all(abs(limits[1:2]) == 180) & diff(limits[3:4]) <= 60) |
      all(abs(limits[1:2]) == 0)
    ) { ## Fix the problem with limits ending up on a single line in polar maps
      
      limits <- c(sort(limits[1:2]), sort(limits[3:4]))
      
      tmp <- define_shapefiles(limits)
      
      if(grepl("antarcticstereographic", tmp$shapefile.name, ignore.case = TRUE) & tmp$decimal.degree.limits) {
        
        limits <- max(limits[3:4])
        polarMap <- TRUE
        message("All decimal degree limits along a single line. You wanted a polar map with latitude limit furthest from the South Pole, right?")
        
      } else if (grepl("arcticstereographic", tmp$shapefile.name, ignore.case = TRUE) & tmp$decimal.degree.limits) {
        
        limits <- min(limits[3:4])
        polarMap <- TRUE
        message("All decimal degree limits along a single line. You wanted a polar map with latitude limit furthest from the North Pole, right?")
        
      }
      
    } else { # Rectangular maps, length(limits) == 4
      
      if(identical(limits[1], limits[2])) stop("Longitude limits[1:2] equal. Cannot plot a map")
      
      if(!decLimits){ # Limits given as UTM coordinates
        
        if(rotate) {
          message("The map rotation does not work for projected coordinates yet. Changed rotate to FALSE")
          rotate <- FALSE
        }
        
        if(identical(limits[3], limits[4])) stop("Latitude limits[3:4] equal. Cannot plot a map")
        
        limits <- c(sort(limits[1:2]), sort(limits[3:4]))
        
        if(is.null(shapefiles) & !is.null(data)) { # Define shapefiles with help of data
          tmp <- data[unname(guess_coordinate_columns(data))]
          ddLimits <- c(range(data[[1]], na.rm = TRUE), range(data[[2]], na.rm = TRUE))
          shapefile.def <- define_shapefiles(ddLimits)
          shapefile.name <- shapefile.def$shapefile.name
          shapefiles <- shapefile_list(shapefile.name)
          
        } else if(is.null(shapefiles)) {
          stop("Cannot detect the required shapefiles automatically from projected limits coordinates. Change the limits to decimal degrees, provide data with decimal degree information or specify the shapefiles argument.")
        }
        
        clipLimits <-
          auto_limits(data =
                        expand.grid(data.frame(
                          lon = limits[1:2],
                          lat = limits[3:4])
                        ),
                      lon = "lon", lat = "lat",
                      proj.in =
                        if(class(shapefiles$land) == "SpatialPolygonsDataFrame") {
                          raster::crs(shapefiles$land)
                        } else {
                          convert_crs(shapefiles$crs)
                        },
                      proj.out = 4326,
                      verbose = verbose)
        
      } else { # Limits given as decimal degrees
        
        limits <- c(limits[1:2], sort(limits[3:4]))
        
        tmp <- dd_to_deg(limits[1:2])
        
        if(tmp[1] > tmp[2]) {
          lonDiff <- 360 - tmp[1] + tmp[2]
        } else {
          lonDiff <- tmp[2] - tmp[1]
        }
        
        midLon <- tmp[1] + lonDiff/2
        midLon <- deg_to_dd(midLon)
        
        if(is.null(shapefiles)) {
          clipLimits <-
            auto_limits(data = expand.grid(
              lon = sort(c(limits[1:2], midLon)),
              lat = limits[3:4]),
              lon = "lon", lat = "lat",
              verbose = verbose)
        } else {
          clipLimits <-
            auto_limits(data = expand.grid(
              lon = sort(c(limits[1:2], midLon)),
              lat = limits[3:4]),
              lon = "lon", lat = "lat",
              proj.out =
                ifelse(grepl("SpatialPolygons", class(shapefiles$land)),
                       sp::proj4string(shapefiles$land),
                       sp::proj4string(eval(parse(text = shapefiles$land)))
                ),
              verbose = verbose)
        }
        
      }
    }
    
    # Shapefile definitions
    
    if(is.null(shapefiles)) {
      shapefile.def <- define_shapefiles(limits)
      shapefile.name <- shapefile.def$shapefile.name
    }
    
  }
  
  # 3. data argument defines the limits and shapefiles if not specified. Also helps the limits argument to find shapefile if it was given as projected coordinates ###
  
  if(!is.null(data) & is.null(limits)) {
    
    if(rotate) {
      
      clipLimits <- auto_limits(data, verbose = verbose)
      limits <- clipLimits$ddLimits
      decLimits <- is_decimal_limit(limits)
      
      tmp <- dd_to_deg(limits[1:2])
      
      if(tmp[1] > tmp[2]) {
        lonDiff <- 360 - tmp[1] + tmp[2]
      } else {
        lonDiff <- tmp[2] - tmp[1]
      }
      
      midLon <- tmp[1] + lonDiff/2
      midLon <- deg_to_dd(midLon)
      
    } else {
      
      if(!is.null(shapefiles)) {
        clipLimits <-
          auto_limits(data, verbose = verbose,
                      proj.out = raster::crs(shapefiles$land),
                      expand.factor = expand.factor
          )
      } else {
        clipLimits <- auto_limits(data, expand.factor = expand.factor, verbose = verbose)
      }
      
      limits <- clipLimits$ddLimits
      decLimits <- is_decimal_limit(limits)
      
    }
    
    if(is.null(shapefiles)) {
      if(!exists("shapefile.name")) {
        if(!decLimits) stop("Cannot detect the required shapefiles automatically from projected coordinates. Change the limits to decimal degrees or specify the shapefiles argument.")
        
        shapefile.def <- define_shapefiles(limits)
        shapefile.name <- shapefile.def$shapefile.name
      }
    }
  }
  
  # 4. Define the shapefiles to be used in the map ####
  
  if(!shapefilesDefined) {
    
    if(exists("shapefile.name") & is.null(shapefiles)) shapefiles <- shapefile_list(shapefile.name)
    
    shapefiles$land <- eval(parse(text = shapefiles$land))
    
    if(glaciers) {
      shapefiles$glacier <- eval(parse(text = shapefiles$glacier))
    } else {
      shapefiles$glacier <- NULL
    }
    
    if(bathymetry) {
      shapefiles$bathy <- eval(parse(text = shapefiles$bathy))
    } else {
      shapefiles$bathy <- NULL
    }
    
  } else {
    
    if(!glaciers) shapefiles$glacier <- NULL
    if(!bathymetry) shapefiles$bathy <- NULL
    
  }
  
  ##  Define the CRS for the underlying data ###
  
  LandCRS <- sf::st_crs(shapefiles$land) # old: raster::crs(shapefiles$land)
  
  # 5. Crop and rotate shapefiles if needed ###
  
  if(is.null(limits)) { # For cases when limits and data are not defined
    
    if(rotate) message("Rotating maps defined using only the shapefiles argument have not been implemented. Changed rotate to FALSE")
    
    # Define map limits
    
    tmp <- sapply(shapefiles[c("land", "glacier", "bathy")], function(k) {
      if(class(k) %in% c("SpatialPolygonsDataFrame", "SpatialPolygons")) {
        length(k) > 0
      } else {
        FALSE
      }
    })
    
    if(all(!tmp)) stop("No shapefile layers to plot.")
    
    
    tmp <- lapply(shapefiles[names(tmp[tmp])], function(k) {
      # out <- raster::extent(k)
      matrix(sf::st_bbox(k), nrow = 1)
    })
    
    tmp <- as.data.frame(do.call(rbind, tmp))
    map.limits <- c(min(tmp[1]), max(tmp[3]), min(tmp[2]), max(tmp[4]))
    
    clipLimits <-
      auto_limits(data = expand.grid(
        lon = c(tmp[[1]], tmp[[3]]),
        lat = c(tmp[[2]], tmp[[4]])),
        lon = "lon",
        lat = "lat",
        proj.in = LandCRS,
        proj.out = 4326
      )
    
  } else if(polarMap) { # Polar maps
    
    if(rotate) message("Rotating polar maps has not been implemented. Changed rotate to FALSE")
    
    # Clip the shapefiles
    
    landBoundary <- clip_shapefile(shapefiles$land, 
                                   limits = limits, 
                                   proj.limits = convert_crs(4326), 
                                   return.boundary = TRUE)
    
    shapefiles$land <- landBoundary$shapefile
    if(glaciers) shapefiles$glacier <- clip_shapefile(shapefiles$glacier, limits = limits, proj.limits = ifelse(decLimits, convert_crs(4326), LandCRS))
    if(bathymetry) shapefiles$bathy <- clip_shapefile(shapefiles$bathy, limits = limits, proj.limits = ifelse(decLimits, convert_crs(4326), LandCRS))
    
    # Define map limits
    
    map.limits <- raster::extent(landBoundary$boundary)[1:4]
    
  } else { # Square maps
    
    if(rotate) {
      if(!shapefiles$name %in% c("ArcticStereographic", "AntarcticStereographic")) {
        message("The map rotation currently works only for stereographic maps. Changed rotate to FALSE")
        rotate <- FALSE
      }
    }
    
    ## Rotate the shapefiles
    
    if(rotate) {
      LandCRS <- sf::st_crs(gsub("lon_0=0", paste0("lon_0=", midLon), LandCRS$proj4string))
      clipLimits <- auto_limits(data =
                                  expand.grid(
                                    lon = sort(c(limits[1:2], midLon)),
                                    lat = limits[3:4]),
                                lon = "lon", lat = "lat",
                                proj.out = LandCRS)
      
      # (These conversions will disappear once the package is entirely sf based)
      shapefiles$land <- 
        sf::as_Spatial( 
          sf::st_transform(sf::st_as_sf(shapefiles$land), LandCRS)
        )
      
      if(glaciers) shapefiles$glacier <- sf::as_Spatial(sf::st_transform(sf::st_as_sf(shapefiles$glacier), LandCRS))
      if(bathymetry) shapefiles$bathy <- sf::as_Spatial(sf::st_transform(sf::st_as_sf(shapefiles$bathy), LandCRS))
      
    }
    
    # Clip the shapefiles
    
    shapefiles$land <- clip_shapefile(shapefiles$land,
                                      limits = clipLimits$projBound,
                                      proj.limits = clipLimits$proj.out
    )
    
    if(glaciers) shapefiles$glacier <- clip_shapefile(shapefiles$glacier, limits = clipLimits$projBound, proj.limits = clipLimits$proj.out)
    if(bathymetry) {
      shapefiles$bathy <- clip_shapefile(shapefiles$bathy, limits = clipLimits$projBound, proj.limits = clipLimits$proj.out)
      shapefiles$bathy@data <- droplevels(shapefiles$bathy@data)
    }
    
    # Define map limits
    
    map.limits <- clipLimits$projLimits
    
  }
  
  # 6. Define the grid lines ####
  
  ## A quick fix. Improve later
  
  if(exists("clipLimits")) {
    if(abs(clipLimits$ddLimits[4]) != 90) {
      tmp <- sp::spTransform(clipLimits$projBound, sp::CRS(convert_crs(4326)))@bbox
      clipLimits$ddLimits <- unname(c(sort(tmp[1,]), sort(tmp[2,])))
    }
  }
  
  ## Define intervals if not specified
  
  if(is.null(lat.interval)) {
    
    if(polarMap) {
      latDist <- 90 - abs(limits)
    } else {
      latDist <- abs(diff(round(clipLimits$ddLimits)[3:4]))
    }
    lat.interval <- ifelse(latDist >= 30, 10, ifelse(latDist >= 15, 5, ifelse(latDist >= 10, 4, ifelse(latDist >= 6, 3, ifelse(latDist > 4, 2, 1)))))
  }
  
  if(is.null(lon.interval)) {
    
    if(polarMap) {
      lon.interval <- 45
    } else {
      if(diff(clipLimits$ddLimits[1:2]) == 360) {
        lonDist <- 360
      } else {
        tmp <- dd_to_deg(round(clipLimits$ddLimits)[1:2])
        
        if(tmp[1] > tmp[2]) {
          lonDist <- 360 - tmp[1] + tmp[2]
        } else {
          lonDist <- tmp[2] - tmp[1]
        }
      }
      
      lon.interval <- ifelse(lonDist > 180, 45, ifelse(lonDist > 90, 30, ifelse(lonDist >= 40, 10, ifelse(lonDist > 10, 5, ifelse(lonDist > 4, 2, 1)))))
    }
  }
  
  ## Define the grid lines based on intervals
  
  if(polarMap) {
    
    poleLat <- ifelse(limits > 0, 90, -90)
    
    LonGridLines <- data.frame(lon = rep(seq(-135, 180, lon.interval), each = 2), lat = rep(c(poleLat, limits), 360/lon.interval))
    LatLimitLine <- data.frame(lon = seq(-180, 180, 1), lat = limits)
    
    LatGridLines <- sign(limits) * seq(from = round(abs(limits)) + lat.interval, to = abs(poleLat) - lat.interval, by = lat.interval)
    LatGridLines <- LatGridLines[LatGridLines != limits]
    LatGridLines <- data.frame(lon = rep(seq(-180, 180, 1), length(LatGridLines)), lat = rep(LatGridLines, each = nrow(LatLimitLine)))
    
    mapGrid <- list(lon.grid.lines = LonGridLines, lat.grid.lines = LatGridLines, lat.limit.line = LatLimitLine)
    
  } else {
    
    minLat <- min(clipLimits$ddLimits[3:4])
    maxLat <- max(clipLimits$ddLimits[3:4])
    
    minLat <- ifelse(minLat < 0, -90, round_any(minLat, 10, floor))
    maxLat <- ifelse(maxLat > 0, 90, round_any(maxLat, 10, ceiling))
    
    lat.breaks <- seq(minLat, maxLat, lat.interval)
    lon.breaks <- unique(c(seq(0, 180, lon.interval), seq(-180, 0, lon.interval)))
    mapGrid <- list(lon.breaks = lon.breaks, lat.breaks = lat.breaks)
  }
  
  # Return ####
  
  out <- list(shapefiles = shapefiles, map.limits = map.limits, polar.map = polarMap, map.grid = mapGrid, proj = LandCRS)
  
  class(out) <- "basemapData"
  
  out
}
