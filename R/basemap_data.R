#' @title Create basemapData object for basemap plotting
#' @description Internal function to create a \code{basemapData} object for \code{\link{basemap}}
#' @inheritParams basemap
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. Common users do not need to worry about these details.
#' @return A list of class \code{basemapData} containing information required for plotting a \code{\link{basemap}}.
#' @keywords internal
#' @export
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}

# Test paramters
# limits = NULL; data = NULL; shapefiles = NULL; bathymetry = FALSE; glaciers = FALSE; lon.interval = NULL; lat.interval = NULL; expand.factor = 1.1; rotate = FALSE; verbose = TRUE
basemap_data <- function(limits = NULL, data = NULL, shapefiles = NULL, crs = NULL, bathymetry = FALSE, bathy.type = NULL, downsample = 0, glaciers = FALSE, lon.interval = NULL, lat.interval = NULL, expand.factor = 1.1, rotate = FALSE, verbose = FALSE) {
  
  ## For code-readability and debugging, the function has been cut to compartments.
  ## The internal functions can be found at the end of this script.
  
  ## Turn sf limits ggOceanMaps compatible and sort ggOceanMaps latitude limits
  
  if(length(limits) == 4 & all(c("xmin", "xmax", "ymin", "ymax") %in% names(limits))) {
    limits <- limits[c("xmin", "xmax", "ymin", "ymax")]
  }
  
  if(length(limits) == 4 & is.null(names(limits))) {
    limits <- c(limits[1:2], sort(limits[3:4]))
    names(limits) <- c("xmin", "xmax", "ymin", "ymax")
  }
  
  # 1. Define the shapefiles ####
  
  x <- basemap_data_define_shapefiles(
    limits = limits, data = data, shapefiles = shapefiles, crs = crs, 
    bathymetry = bathymetry, bathy.type = bathy.type, downsample = downsample, glaciers = glaciers, 
    rotate = rotate, expand.factor = expand.factor, verbose = FALSE
  )
  
  # 2. Crop and rotate shapefiles if needed ####
  
  x <- basemap_data_crop(x = x, bathymetry = bathymetry, glaciers = glaciers, crs = crs)
  
  # 3. Define the grid lines ####
  
  x <- basemap_define_grid_lines(x = x, lon.interval = lon.interval, lat.interval = lat.interval)
  
  # Return ####
  
  out <- list(shapefiles = x$shapefiles, map.limits = x$map_limits, 
              polar.map = x$polarMap, map.grid = x$mapGrid, proj = x$crs)
  
  class(out) <- "basemapData"
  
  out
}



# Internal functions to make the code easier to read and debug ####

## Detect case from provided arguments ####

basemap_data_detect_case <- function(limits = NULL, data = NULL, shapefiles = NULL) {
  
  if(is.null(limits) & is.null(data) & is.null(shapefiles)) {
    "none"
  } else if(!is.null(limits)) {
    
    if(!is.numeric(limits)) stop("Limits have to be given as numeric vectors of length 1 or 4. See Details.")
    if(!length(limits) %in% c(1, 4)) stop("Limits has to be either length of 1 or 4. See Details.")
    
    decLimits <- is_decimal_limit(limits)
    
    if(length(limits) == 1) {
      if(!decLimits) stop("Limits of length 1 have to be given as decimal degrees.")
      
      if(abs(limits) <= 89 & abs(limits) >= 10) {
        "limits_polar"
      } else {
        stop("The limits argument has to be between 10 and 89 (or negative) for polar stereographic maps.")
      }
    } else if(
      (all(abs(limits[1:2]) == 180) & diff(limits[3:4]) <= 60) |
      all(abs(limits[1:2]) == 0)
    ) { ## Fix the problem with limits ending up on a single line in polar maps
      "limits_polar_adjust"
    } else if(decLimits) {
      if(identical(limits[1], limits[2])) stop("Longitude limits[1:2] equal. Cannot plot a map")
      if(identical(limits[3], limits[4])) stop("Latitude limits[3:4] equal. Cannot plot a map")
      "limits_dec"
    } else {
      if(identical(limits[1], limits[2])) stop("Longitude limits[1:2] equal. Cannot plot a map")
      if(identical(limits[3], limits[4])) stop("Latitude limits[3:4] equal. Cannot plot a map")
      "limits_proj"
    }
    
    
  } else if(!is.null(data)) {
    if(inherits(data, c("sf", "sfc"))) {
      "data_sf"
    } else if(inherits(data, c("SpatialPolygons", "SpatialPolygonsDataFrame","SpatialPoints", "SpatialPointsDataFrame"))) {
      "data_sp"
    } else if(inherits(data, c("data.frame", "tibble", "data.table"))) {
      clipLimits <- try(auto_limits(data, verbose = FALSE), silent = TRUE)
      
      if(inherits(clipLimits, "try-error")) {
        decLimits <- FALSE
      } else {
        decLimits <- is_decimal_limit(clipLimits$ddLimits)
      }
      
      if(decLimits) {
        "data_dec"
      } else {
        "data_proj"
      }
    } else {
      stop("The data argument has to be either a data frame, tibble, data.table, sf or sp object.")
    }
    
  } else {
    "shapefiles"
  }
}


## Define shapefiles ####
basemap_data_define_shapefiles <- function(limits = NULL, data = NULL, shapefiles = NULL, crs = NULL, bathymetry = FALSE, bathy.type = NULL, downsample = 0, glaciers = FALSE, rotate = FALSE, expand.factor = 1.1, verbose = FALSE) {
  
  # Switches and checks ####
  
  shapefilesDefined <- FALSE
  polarMap <- FALSE
  
  # 1. Detect case ####
  
  case <- basemap_data_detect_case(limits = limits, data = data, shapefiles = shapefiles)
  
  # 2. Define shapefiles when specified ####
  
  if(case == "none") shapefiles <- "DecimalDegree"
  
  if(!is.null(shapefiles)) {
    
    error_test <- quiet(try(match.arg(shapefiles, shapefile_list("all")$name), silent = TRUE))
    
    if(!inherits(error_test, "try-error")) {
      shapefiles <- shapefile_list(shapefiles)
      
      if(is.null(limits) & is.null(data)) {
        limits <- shapefiles$limits
      }
      
      if(is.na(shapefiles$glacier) & glaciers) {
        message(shapefiles$name, " does not contain glaciers. Switched to FALSE")
        glaciers <- FALSE
      }
      
      if(!glaciers) shapefiles$glacier <- NULL
      if(!bathymetry) {
        shapefiles$bathy <- NULL
      } else {
        shapefiles$bathy <- shapefiles$bathy[bathy.type]
      }
      
      ## Load the shapefiles if download required
      
      shapefiles <- load_map_data(shapefiles)
      
      # if(!is.na(shapefiles$path) & 
      #    any(sapply(
      #      shapefiles[names(shapefiles) %in% c("land", "glacier", "bathy")], 
      #      function(k) !grepl("::", k) & !is.null(k)))
      # ) {
      #   
      #   ## Predifined shapefile case
      #   
      #   tmp <- load_map_data(x = shapefiles)
      #   
      #   shapefiles <- stats::setNames(lapply(seq_along(shapefiles), function(i) {
      #     test <- which(names(tmp) %in% shapefiles[[i]])
      #     
      #     if(length(test) != 1) {
      #       shapefiles[[i]]
      #     } else {
      #       if(names(shapefiles)[i] == "glacier" & !glaciers) {
      #         NULL
      #       } else if(names(shapefiles)[i] == "bathy" & !bathymetry) {
      #         NULL
      #       } else {
      #         tmp[[test]]
      #       }
      #     }
      #   }), names(shapefiles))
      # }
      
      # if(is.character(shapefiles$land)) {
      #   shapefiles$land <- eval(parse(text = shapefiles$land))
      # }
      # 
      # if(is.character(shapefiles$glacier)) {
      #   shapefiles$glacier <- eval(parse(text = shapefiles$glacier))
      # }
      # 
      # if(is.character(shapefiles$bathy)) {
      #   shapefiles$bathy <- eval(parse(text = shapefiles$bathy))
      # }
      
      if(any(sapply(shapefiles, function(k) 
        inherits(k, c("SpatialPolygons", "SpatialPolygonsDataFrame"))))) {
        shapefiles <- lapply(shapefiles, function(k) {
          if(inherits(k, c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
            sf::st_as_sf(k)
          } else {
            k
          }
        })
      }
      
      shapefilesDefined <- TRUE
      
    } else {
      
      ##Custom shapefile case
      
      shapefiles <- shapefiles[c("land", "glacier", "bathy")]
      shapefiles <- shapefiles[!is.na(names(shapefiles))]
      
      if(any(sapply(shapefiles, function(k) 
        inherits(k, c("SpatialPolygons", "SpatialPolygonsDataFrame"))))) {
        shapefiles <- lapply(shapefiles, function(k) {
          if(inherits(k, c("SpatialPolygons", "SpatialPolygonsDataFrame"))) {
            sf::st_as_sf(k)
          } else {
            k
          }
        })
      }
      
      customShapefiles <- sapply(shapefiles, function(k) class(k)[1])
      
      if(all(sapply(customShapefiles, function(k) is.null(k)))) stop("One of following shapefiles elements 'land', 'glacier', and 'bathy' must be a an sf or stars object. See Details.")
    }
    # if(any(sapply(shapefiles, function(k) !inherits(k, c("NULL", "sf", "SpatialPolygonsDataFrame", "SpatialPolygons"))))) stop("Shapefiles elements 'land', 'glacier', and 'bathy' must either be a SpatialPolygonsDataFrame, SpatialPolygons, or NULL. See Details.")
    
    shapefilesDefined <- TRUE
  }
  
  # 3. Define clip limits for shapefiles ####
  
  ## shapefiles ####
  
  if(case %in% c("shapefiles", "none")) { 
    
    clip_shape <- sf::st_as_sfc(sf::st_bbox(shapefiles$land))
    
    if(!is.null(shapefiles$name) && shapefiles$name %in% shapefile_list("all")$name) {
      clip_shape <- shapefile_list(shapefiles$name)$limits
      crs <- sf::st_crs(shapefile_list(shapefiles$name)$crs)
      
      
      if(length(clip_shape) == 4) {
        limits <- stats::setNames(clip_shape, c("xmin", "xmax", "ymin", "ymax"))
        
        if(!sf::st_is_longlat(crs)){
          clip_shape <- sf::st_as_sfc(
            sf::st_bbox(limits, crs = crs)
          )
          
          limits <- sf::st_bbox(
            sf::st_transform(
              clip_shape, crs = 4326))[c("xmin", "xmax", "ymin", "ymax")]
        }
        
      } else {
        limits <- clip_shape
        polarMap <- TRUE
        rotate <- FALSE
      }
      
    } else if(sf::st_is_longlat(clip_shape)) {
      limits <- sf::st_bbox(clip_shape)[c("xmin", "xmax", "ymin", "ymax")]
      crs <- sf::st_crs(shapefiles$land)
    } else {
      limits <- sf::st_bbox(sf::st_transform(clip_shape, crs = 4326))[c("xmin", "xmax", "ymin", "ymax")]
      crs <- sf::st_crs(shapefiles$land)
    }
    
    if(rotate & is.numeric(limits) & length(limits) == 4) {
      crs <- rotate_crs(crs, limits[1:2])
      clip_shape <- dd_clip_boundary(limits, crs, expand.factor = 1.1)
    } 
    
    ## polar ####
  } else if(case %in% c("limits_polar", "limits_polar_adjust")) {
    
    if(case %in% c("limits_polar_adjust")) {
      tmp <- define_shapefiles(limits)
      
      if(grepl("antarcticstereographic", tmp$shapefile.name, ignore.case = TRUE) & tmp$decimal.degree.limits) {
        
        limits <- max(limits[3:4])
        message("All decimal degree limits along a single line. You wanted a polar map with latitude limit furthest from the South Pole, right?")
        
      } else if (grepl("arcticstereographic", tmp$shapefile.name, ignore.case = TRUE) & tmp$decimal.degree.limits) {
        
        limits <- min(limits[3:4])
        message("All decimal degree limits along a single line. You wanted a polar map with latitude limit furthest from the North Pole, right?")
        
      } else {
        stop("limits_polar_adjust definition did not work as expected")
      }
    }
    
    polarMap <- TRUE
    clip_shape <- limits
    
    if(is.null(shapefiles)) {
      shapefile.def <- define_shapefiles(limits, force_dd = TRUE)
      if(!is.null(crs)) message("Polar maps have a defined crs. Cannot provide a custom one.")
      crs <- sf::st_crs(shapefile.def$crs)
      shapefile.name <- shapefile.def$shapefile.name
      shapefiles <- shapefile_list(shapefile.name)
      
    } else {
      if(inherits(shapefiles$land, c("sf", "SpatialPolygonsDataFrame", "SpatialPolygons"))) {
        if(inherits(shapefiles$land, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
          shapefiles$land <- sf::st_as_sf(shapefiles$land)
        }
        crs <- suppressWarnings(sf::st_crs(shapefiles$land))
      } else {
        crs <- suppressWarnings(sf::st_crs(eval(parse(text = shapefiles$land))))
      }
    }
    
  } else if(case %in% c("limits_dec")) { 
    ### limits_dec ####
    
    # Shapefile definitions
    
    if(is.null(shapefiles)) {
      shapefile.def <- define_shapefiles(limits, force_dd = TRUE)
      if(is.null(crs)) {crs <- sf::st_crs(shapefile.def$crs)}
      shapefile.name <- shapefile.def$shapefile.name
      shapefiles <- shapefile_list(shapefile.name)
      
    } else {
      if(inherits(shapefiles$land, c("sf", "SpatialPolygonsDataFrame", "SpatialPolygons"))) {
        if(inherits(shapefiles$land, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
          shapefiles$land <- sf::st_as_sf(shapefiles$land)
        }
        crs <- suppressWarnings(sf::st_crs(shapefiles$land))
      } else {
        crs <- suppressWarnings(sf::st_crs(eval(parse(text = shapefiles$land))))
      }
    }
    
    if(sf::st_is_longlat(crs) && sign(limits[1]) != sign(limits[2]) & !rotate) {
      msg <- paste0("Detecting antimeridian crossing on decimal degree map. Plotting only 
              works with rotate = TRUE. Turning rotate on. Adjust limits if this
              is not desired.")
      
      message(paste(strwrap(msg), collapse= "\n"))
      rotate <- TRUE
    }
    
    if(rotate) {
      crs <- rotate_crs(crs, limits[1:2])
    }
    
    clip_shape <- dd_clip_boundary(limits, crs)
    
    
    # } else if(case %in% c("data_dec")) { ### data frames ###
    #   
    #   if(is.null(shapefiles)) {
    #     
    #     
    #     
    #     
    #     
    #     if(rotate){
    #       tmp <- auto_limits(data, verbose = verbose)
    # 
    #       tmp2 <- guess_coordinate_columns(data)
    #       tmp$ddLimits <- stats::setNames(
    #         c(deg_to_dd(range(dd_to_deg(data[[tmp2[names(tmp2) == "lon"]]]))),
    #           range(data[[tmp2[names(tmp2) == "lat"]]])
    #           ),
    #         c("xmin", "xmax", "ymin", "ymax")
    #       )
    #     } else {
    #       tmp <- auto_limits(data, expand.factor = 1.1, verbose = verbose)
    #     }
    #     
    #     shapefile.def <- define_shapefiles(tmp$ddLimits, force_dd = TRUE)
    #     if(is.null(crs)) {crs <- sf::st_crs(shapefile.def$crs)}
    #     shapefile.name <- shapefile.def$shapefile.name
    #     shapefiles <- shapefile_list(shapefile.name)
    #     # clip_shape <- tmp$projBound
    #     # 
    #   } else {
    #     if(inherits(shapefiles$land, c("sf", "SpatialPolygonsDataFrame", "SpatialPolygons"))) {
    #       if(inherits(shapefiles$land, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
    #         shapefiles$land <- sf::st_as_sf(shapefiles$land)
    #       }
    #       crs <- suppressWarnings(sf::st_crs(shapefiles$land))
    #     } else {
    #       crs <- suppressWarnings(sf::st_crs(eval(parse(text = shapefiles$land))))
    #     }
    #     
    #     tmp <- auto_limits(data, proj.out = crs, expand.factor = 1.1, verbose = verbose)
    #     clip_shape <- sf::st_transform(tmp$projBound, crs)
    #   }
    #   
    #   
    #   # if(rotate) {
    #   #   limits <- tmp$ddLimits
    #   # } else {
    #     limits <- sf::st_bbox(sf::st_transform(tmp$projBound, crs = 4326))[c("xmin", "xmax", "ymin", "ymax")]
    #     
    #     if(rotate) {
    #       crs <- rotate_crs(crs, limits[1:2])
    #     }
    #     
    #     clip_shape <- dd_clip_boundary(limits, crs)
    #   # }
    #   
    #   #limits <- tmp$ddLimits
    #   # limits <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(clip_shape)), crs = 4326))[c("xmin", "xmax", "ymin", "ymax")]
    #   
    #   if(sf::st_is_longlat(crs) && sign(limits[1]) != sign(limits[2]) && diff(limits[1:2]) < 180 & !rotate) {
    #     msg <- paste0("Detecting antimeridian crossing on decimal degree map. Plotting only 
    #             works with rotate = TRUE. Turning rotate on. Adjust limits if this
    #             is not desired.")
    #     
    #     message(paste(strwrap(msg), collapse= "\n"))
    #     rotate <- TRUE
    #   }
    #   
    #   if(rotate) {
    #     crs <- rotate_crs(crs, limits[1:2])
    #     clip_shape <- dd_clip_boundary(limits, crs, expand.factor = 1.1)
    #   } else {
    #     clip_shape <- dd_clip_boundary(limits, crs)
    #   }
    #   
  } else if(case %in% c("data_sf", "data_sp", "data_dec")) { ### data ####
    
    if(case == "data_sp") data <- sf::st_as_sf(data)
    
    if(case == "data_dec") {
      tmp <- guess_coordinate_columns(data)
      data <- sf::st_as_sf(data, coords = tmp, crs = 4326)
    }
    
    if(is.null(shapefiles)) {
      
      if(rotate) {
        tmp <- sf::st_coordinates(sf::st_transform(data, 4326))
        
        limits <- stats::setNames(
          c(deg_to_dd(range(dd_to_deg(tmp[,1]))), range(tmp[,2])),
          c("xmin", "xmax", "ymin", "ymax"))
        
      } else if(!sf::st_is_longlat(data)) {
        limits <- sf::st_bbox(sf::st_transform(data, 4326))[c("xmin", "xmax", "ymin", "ymax")] 
      } else {
        limits <- sf::st_bbox(data)[c("xmin", "xmax", "ymin", "ymax")] 
      }
      
      shapefile.def <- define_shapefiles(limits, force_dd = TRUE)
      if(is.null(crs)) crs <- sf::st_crs(shapefile.def$crs)
      shapefile.name <- shapefile.def$shapefile.name
      shapefiles <- shapefile_list(shapefile.name)
      
    } else {
      if(inherits(shapefiles$land, c("sf", "SpatialPolygonsDataFrame", "SpatialPolygons"))) {
        if(inherits(shapefiles$land, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
          shapefiles$land <- sf::st_as_sf(shapefiles$land)
        }
        crs <- suppressWarnings(sf::st_crs(shapefiles$land))
      } else {
        crs <- suppressWarnings(sf::st_crs(eval(parse(text = shapefiles$land))))
      }
      
      # clip_shape <- sf::st_as_sfc(sf::st_bbox(sf::st_transform(data, crs)))
      limits <- sf::st_bbox(sf::st_transform(data, 4326))[c("xmin", "xmax", "ymin", "ymax")]
    }
    
    if(rotate) {
      crs <- rotate_crs(crs, limits[1:2])
      clip_shape <- dd_clip_boundary(limits, crs, expand.factor = 1.1)
    } else {
      if(sf::st_crs(data) == crs) {
        clip_shape <- sf::st_as_sfc(sf::st_bbox(data))
        clip_shape <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(clip_shape, dist = 0.01*sqrt(sf::st_area(clip_shape)))))
      } else {
        clip_shape <- sf::st_as_sfc(sf::st_bbox(sf::st_transform(data, crs)))
        clip_shape <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(clip_shape, dist = 0.01*sqrt(sf::st_area(clip_shape)))))
      }
    }
    
  } else if(case %in% c("limits_proj", "data_proj")) { ## Projected limits/data ####
    
    if(case %in% c("data_proj")) { ### Data frames in decimal degrees ###
      if(is.null(shapefiles)) {
        stop("Cannot detect the required shapefiles automatically from projected coordinates. Change the limits to decimal degrees or specify the shapefiles argument.")
      }
      if(inherits(shapefiles$land, c("sf", "SpatialPolygonsDataFrame", "SpatialPolygons"))) {
        if(inherits(shapefiles$land, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
          shapefiles$land <- sf::st_as_sf(shapefiles$land)
        }
        crs <- suppressWarnings(sf::st_crs(shapefiles$land))
      } else {
        crs <- suppressWarnings(sf::st_crs(eval(parse(text = shapefiles$land))))
      }
      
      limits <- auto_limits(data, proj.in = crs, proj.out = 4326, verbose = verbose)$ddLimits
      names(limits) <- c("xmin", "xmax", "ymin", "ymax")
    }
    
    
    if(is.null(shapefiles) & !is.null(data)) { # Define shapefiles with help of data
      tmp <- data[unname(guess_coordinate_columns(data))]
      ddLimits <- c(range(data[[1]], na.rm = TRUE), range(data[[2]], na.rm = TRUE))
      
      shapefile.def <- define_shapefiles(ddLimits, force_dd = TRUE)
      if(is.null(crs)) {crs <- sf::st_crs(shapefile.def$crs)}
      shapefile.name <- shapefile.def$shapefile.name
      shapefiles <- shapefile_list(shapefile.name)
      
    } else if(!is.null(shapefiles)) {
      
      if(inherits(shapefiles$land, c("sf", "sfc", "SpatialPolygonsDataFrame", "SpatialPolygons"))) {
        if(inherits(shapefiles$land, c("SpatialPolygonsDataFrame", "SpatialPolygons"))) {
          shapefiles$land <- sf::st_as_sf(shapefiles$land)
        }
        crs <- suppressWarnings(sf::st_crs(shapefiles$land))
      } else {
        crs <- suppressWarnings(sf::st_crs(eval(parse(text = shapefiles$land))))
      }
      
    } else {
      stop("Cannot detect the required shapefiles automatically from projected limits coordinates. Change the limits to decimal degrees, provide data with decimal degree information or specify the shapefiles argument.")
    }
    
    clipLimits <-
      auto_limits(data =
                    expand.grid(data.frame(
                      lon = limits[1:2],
                      lat = limits[3:4])
                    ),
                  lon = "lon", lat = "lat",
                  proj.in = crs,
                  proj.out = 4326,
                  verbose = verbose)
    
    limits <- clipLimits$ddLimits
    
    if(rotate) {
      crs <- rotate_crs(crs, limits[1:2])
      clip_shape <- dd_clip_boundary(limits, crs, expand.factor = 1.1)
    } else {
      clip_shape <- clipLimits$projBound
    }
  } else {
    stop("Unrecognized case")
  }
  
  # 4. Define shapefiles ####
  
  if(!shapefilesDefined) {
    
    if(exists("shapefile.name") & is.null(shapefiles)) shapefiles <- shapefile_list(shapefile.name)
    
    if(!glaciers) shapefiles$glacier <- NULL
    if(!bathymetry) {
      shapefiles$bathy <- NULL
    } else {
      if(bathy.type == "raster_user" && is.na(shapefiles$bathy[bathy.type])) {
        msg <- "Define path to the user defined bathymetry raster using options(ggOceanMaps.userpath = 'PATH_TO_THE_FILE'))"
        stop(paste(strwrap(msg), collapse= "\n"))
      }
      
      shapefiles$bathy <- shapefiles$bathy[bathy.type]
    }
    
    shapefiles <- load_map_data(shapefiles, downsample = downsample)
    
  }
  
  # 5. Return ####
  
  list(shapefiles = shapefiles, limits = limits, polarMap = polarMap, clip_limits = clip_shape, crs = crs, rotate = rotate, case = case)
  
}





##################### #
## Crop shapefiles ####

basemap_data_crop <- function(x, bathymetry = FALSE, glaciers = FALSE, crs = NULL) {
  
  # 1. Clip shapefiles ####
  
  if(x$polarMap) {
    landBoundary <- clip_shapefile(
      sf::st_transform(x$shapefiles$land, crs = x$crs),
      limits = x$clip_limits,
      return.boundary = TRUE
    )
  } else {
    if(!is.null(crs)) { # this hack is required for custom crs. Couldn't come up with a better solution
      landBoundary <- clip_shapefile(
        x$shapefiles$land,
        limits = smoothr::densify(x$clip_limits),
        return.boundary = TRUE
      )
      
      landBoundary <- lapply(landBoundary, function(k) {
        sf::st_transform(k, crs)
      })
      
    } else {
      landBoundary <- clip_shapefile(
        sf::st_transform(x$shapefiles$land, crs = x$crs),
        limits = sf::st_transform(x$clip_limits, crs = x$crs),
        return.boundary = TRUE
      )
    }
  }
  
  x$shapefiles$land <- landBoundary$shapefile
  x$clip_limits <- landBoundary$boundary
  
  # if(x$rotate) {
  #   landBoundary <- clip_shapefile(
  #     sf::st_transform(x$shapefiles$land, crs = x$crs),
  #     limits = sf::st_transform(x$clip_limits, crs = x$crs),
  #     return.boundary = TRUE
  #   )
  #   
  #   x$shapefiles$land <- landBoundary$shapefile
  #   x$clip_limits <- landBoundary$boundary
  #   
  # } else {
  #   
  #   landBoundary <- clip_shapefile(
  #     x$shapefiles$land, 
  #     limits = x$clip_limits, 
  #     return.boundary = TRUE
  #   )
  #   
  #   x$shapefiles$land <- landBoundary$shapefile
  #   
  # }
  # 
  if(glaciers) {
    if(!is.null(crs)) { # this hack is required for custom crs. Couldn't come up with a better solution
      x$shapefiles$glacier <- 
        sf::st_transform(
          clip_shapefile(
            x$shapefiles$glacier,
            limits = sf::st_transform(x$clip_limits, crs = 4326)
          ),
          crs = crs
        )
    } else {
      x$shapefiles$glacier <- clip_shapefile(
        sf::st_transform(x$shapefiles$glacier, crs = x$crs),
        limits = x$clip_limits
      )
    }
  }
  
  if(bathymetry) {
    
    if(inherits(x$shapefiles$bathy, "bathyRaster")) {
      # raster bathymetries
      newgrid <- stars::st_as_stars(sf::st_bbox(x$clip_limits))
      x$shapefiles$bathy$raster <- stars::st_warp(x$shapefiles$bathy$raster, newgrid)
      
      if(x$polarMap) {
        x$shapefiles$bathy$raster <- x$shapefiles$bathy$raster[x$clip_limits]
      }
      
      if(inherits(x$shapefiles$bathy$raster[[1]], "factor")) {
        x$shapefiles$bathy$raster <- droplevels(x$shapefiles$bathy$raster)
      }
    } else if(inherits(x$shapefiles$bathy, c("stars", "stars_proxy"))) {
      
      x$shapefiles$bathy <- raster_bathymetry(
        x$shapefiles$bathy,
        depths = NULL,
        boundary = sf::st_transform(smoothr::densify(x$clip_limits, n = 10), crs = 4326),
        verbose = FALSE
      )
      
      newgrid <- stars::st_as_stars(sf::st_bbox(x$clip_limits))
      x$shapefiles$bathy$raster <- stars::st_warp(x$shapefiles$bathy$raster, newgrid)
      
      if(x$polarMap) {
        x$shapefiles$bathy$raster <- x$shapefiles$bathy$raster[x$clip_limits]
      }
      
    } else {
      # vector bathymetries
      x$shapefiles$bathy <- clip_shapefile(
        sf::st_transform(x$shapefiles$bathy, crs = x$crs),
        limits = x$clip_limits
      )
      
      x$shapefiles$bathy$depth <- droplevels(x$shapefiles$bathy$depth)
    }
  }
  
  if(!x$polarMap) {
    x$limits <- 
      sf::st_bbox(sf::st_transform(sf::st_as_sf(x$clip_limits), 4236))[c("xmin", "xmax", "ymin", "ymax")]
  }
  
  map_limits <- sf::st_bbox(x$clip_limits)[c("xmin", "xmax", "ymin", "ymax")]
  
  # Return ####
  
  list(shapefiles = x$shapefiles, polarMap = x$polarMap, decLimits = x$limits, limit_shape = x$clip_limits, map_limits = map_limits, crs = x$crs)
  
}

####################### #
## Define grid lines ####

basemap_define_grid_lines <- function(x, lon.interval = NULL, lat.interval = NULL) {
  
  ## A quick fix. Improve later
  
  # if(!is.null(x$clipLimits)) {
  #   if(abs(x$clipLimits$ddLimits[4]) != 90) {
  #     tmp <- sf::st_bbox(sf::st_transform(x$clipLimits$projBound, 4326))
  #     x$clipLimits$ddLimits <- tmp[c("xmin", "xmax", "ymin", "ymax")]
  #   }
  # }
  
  ## Define intervals if not specified
  
  if(is.null(lat.interval)) {
    
    if(x$polarMap) {
      latDist <- 90 - abs(x$decLimits)
    } else {
      limits <- sf::st_bbox(sf::st_transform(x$limit_shape, 4326))[c("xmin", "xmax", "ymin", "ymax")]
      
      latDist <- abs(diff(round(limits)[3:4]))
    }
    lat.interval <- 
      ifelse(latDist >= 30, 10, 
             ifelse(latDist >= 15, 5, 
                    ifelse(latDist >= 10, 4, 
                           ifelse(latDist >= 6, 3, 
                                  ifelse(latDist > 4, 2, 1)
                           ))))
  }
  
  if(is.null(lon.interval)) {
    
    if(x$polarMap) {
      lon.interval <- 45
    } else {
      limits <- sf::st_bbox(sf::st_transform(x$limit_shape, 4326))[c("xmin", "xmax", "ymin", "ymax")]
      
      if(diff(limits[1:2]) == 360) {
        lonDist <- 360
      } else {
        tmp <- dd_to_deg(round(x$decLimits)[1:2])
        
        if(tmp[1] > tmp[2]) {
          lonDist <- 360 - tmp[1] + tmp[2]
        } else {
          lonDist <- tmp[2] - tmp[1]
        }
      }
      
      lon.interval <- 
        ifelse(lonDist > 180, 45, 
               ifelse(lonDist > 90, 30, 
                      ifelse(lonDist >= 40, 10, 
                             ifelse(lonDist > 10, 5, 
                                    ifelse(lonDist > 4, 2, 1)
                             ))))
    }
  }
  
  ## Define the grid lines based on intervals
  
  if(x$polarMap) {
    
    poleLat <- ifelse(x$decLimits > 0, 90, -90)
    
    LonGridLines <- data.frame(
      id = rep(1:(360/lon.interval), each = 2),
      lon = rep(seq(-135, 180, lon.interval), each = 2), 
      lat = rep(c(poleLat, x$decLimits), 360/lon.interval))
    
    LonGridLines <- 
      sf::st_sfc(sf::st_multilinestring(
        x = lapply(unique(LonGridLines$id), function(i) {
          sf::st_linestring(as.matrix(LonGridLines[LonGridLines$id == i, 2:3]))
        })
      ), crs = 4326)
    
    LatLimitLine <- data.frame(lon = seq(-180, 180, 1), lat = x$decLimits)
    
    LatGridLines <- 
      sign(x$decLimits) * seq(from = round(abs(x$decLimits)) + lat.interval, 
                              to = abs(poleLat) - lat.interval, by = lat.interval)
    LatGridLines <- LatGridLines[LatGridLines != x$decLimits]
    LatGridLines <- 
      data.frame(lon = rep(seq(-180, 180, 1), length(LatGridLines)), 
                 lat = rep(LatGridLines, each = nrow(LatLimitLine)))
    
    LatGridLines <- sf::st_sfc(sf::st_multilinestring(
      lapply(unique(LatGridLines$lat), function(k) {
        sf::st_linestring(as.matrix(LatGridLines[LatGridLines$lat == k,]))
      })
    ), crs = 4326)
    
    LatLimitLine <- 
      sf::st_sfc(
        sf::st_linestring(
          as.matrix(LatLimitLine)
        ), crs = 4326)
    
    mapGrid <- list(lon.grid.lines = LonGridLines, lat.grid.lines = LatGridLines, lat.limit.line = LatLimitLine)
    
  } else {
    
    limits <- sf::st_bbox(sf::st_transform(x$limit_shape, 4326))[c("xmin", "xmax", "ymin", "ymax")]
    
    minLat <- min(limits[3:4])
    maxLat <- max(limits[3:4])
    
    minLat <- ifelse(minLat < 0, -90, round_any(minLat, 10, floor))
    maxLat <- ifelse(maxLat > 0, 90, round_any(maxLat, 10, ceiling))
    
    lat.breaks <- seq(minLat, maxLat, lat.interval)
    lon.breaks <- unique(c(seq(0, 180, lon.interval), seq(-180, 0, lon.interval)))
    mapGrid <- list(lon.breaks = lon.breaks, lat.breaks = lat.breaks)
  }
  
  # Return ####
  
  list(shapefiles = x$shapefiles, polarMap = x$polarMap, decLimits = x$decLimits, 
       limit_shape = x$limit_shape, map_limits = x$map_limits, 
       crs = x$crs, mapGrid = mapGrid)
  
}
