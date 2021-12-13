#' @title Transform spatial coordinates to another projection
#' @description Transforms spatial coordinates from original projection (decimal degrees assumed) to another projection.
#' @param x Data frame to be transformed. Can be omitted if numeric vectors are assigned to \code{lon} and \code{lat}.
#' @param lon,lat Either a name of the longitude and latitude columns in \code{x} or a numeric vector containing longitude and latitude coordinates. Use \code{NULL} to \link[=guess_coordinate_columns]{guess the longitude and/or latitude columns} in \code{x}.
#' @param new.names Character vector of length 2 specifying the names of transformed longitude and latitude columns, respectively. Alternatively \code{NULL}, which returns column names from \code{x} or "auto", which uses \code{NULL} if \code{bind = FALSE} and \code{c("lon.proj", "lat.proj")} if \code{bind = TRUE}.
#' @param proj.in The original \code{\link[sf:st_crs]{CRS}}. If \code{NULL}, the projection is taken from \code{x}. \code{x} must be a \link[sf:st]{spatial} object in that case.
#' @param proj.out Character. Either \code{NULL}, \code{\link[sf:st_crs]{CRS}} the coordinates should be transformed to or a name of shapefiles in \code{\link{shapefile_list}}. If \code{NULL}, the output projection will be automatically determined from data. This option requires decimal degrees as input option.
#' @param verbose  Logical indicating whether information about the projection should be returned as message. Set to \code{FALSE} to make the function silent.
#' @param bind logical. Should only transformed coordinates be returned (\code{FALSE}, default) or should x be returned with transformed coordinates (\code{TRUE})?
#' @param na character specifying the NA action for missing coordinates. The "ignore" option ignores the coordinates and returns NAs to transformed coordinates. The "remove" option removes missing values from \code{x} returning a message while doing it. Any other character argument will trigger \code{na.fail} stopping the function in case of missing coordinates.
#' @return Returns a data frame with transformed spatial coordinates.
#' @details If \code{x} is specified, the function guesses longitude and latitude columns from \code{x} by default.
#' @family basemap functions
#' @author Mikko Vihtakari
#' @examples
#' # Coordinates are automatically transformed to the pre-made shapefile
#' # projections:
#' x <- data.frame(lon = c(-150, 150), lat = c(60, 90))
#' transform_coord(x)
#' transform_coord(x, bind = TRUE)
#'
#' x <- data.frame(lon = c(-150, 150), lat = c(20, 50))
#' transform_coord(x, bind = TRUE) # no transformation required.
#' @export

## Debug parameters
# x = NULL; lon = NULL; lat = NULL; new.names = "auto"; proj.in = 4326; proj.out = NULL; verbose = FALSE; bind = FALSE; na = "ignore"
# x = data; bind = TRUE; new.names = "auto"; na = "ignore"
transform_coord <- function(x = NULL, lon = NULL, lat = NULL, new.names = "auto", proj.in = 4326, proj.out = NULL, verbose = FALSE, bind = FALSE, na = "ignore") {
  
  # Checks ----
  
  if(length(new.names) == 1) {
    if(new.names == "auto") {
      
      if(is.null(x)) {
        new.names <- c("lon", "lat")
      } else if(bind) {
        new.names <- c("lon.proj", "lat.proj")
      } else {
        new.names <- guess_coordinate_columns(x)
      }
      
    }
  }
  
  if(is.null(proj.out) & !sf::st_is_longlat(proj.in)) stop("proj.in has to be decimal degrees when proj.out = NULL.")
  
  if(!is.null(proj.out)) {
    
    error_test <- quiet(try(match.arg(proj.out, shapefile_list("all")$name), silent = TRUE))
    
    if(class(error_test) != "try-error") {
      proj.out <- sf::st_crs(shapefile_list(proj.out)$crs)
    }
  }
  
  if(is.null(proj.in)) {
    if(is.null(x)) stop("a spatial object as x is required when proj.in = NULL")
    
    if("sf" %in% class(x)) {
      proj.in <- sf::st_crs(x) 
    } else if("sp" %in% class(x)) {
      proj.in <- raster::crs(x)
    } else stop("a spatial object of class sf or sp as x is required when proj.in = NULL")
  }
  
  ## Case for defined x and undefined lon or/and lat
  if(!is.null(x) & (is.null(lon) | is.null(lat))) {
    # if(!"data.frame" %in% class(x)) stop("x argument has to be a data.frame or NULL")
    
    tmp <- guess_coordinate_columns(x)
    
    lon <- unname(tmp[names(tmp) == "lon"])
    lat <- unname(tmp[names(tmp) == "lat"])
    
    if(verbose) {
      message(paste0("Used ", lon, " and ", lat, " as input coordinate column names in x"))
    }
  }
  
  if(is.null(x) & (!is.numeric(lon) | !is.numeric(lat))) {
    stop("Define either x or lon and lat as numeric vectors")
  }
  
  ## Convert tibble to data.frame
  # if(any(class(x) == "tbl")) {
  #   x <- as.data.frame(x)
  # }
  
  ## Make the data frame ---
  
  if(is.null(x) & (is.numeric(lon) | is.numeric(lat))) {
    if(length(lon) != length(lat)) stop("lat and lon must be of equal length")
    y <- data.frame(lon = lon, lat = lat)
    lon <- "lon"; lat <- "lat"
    y$id <- 1:nrow(y)
  }
  
  if(!is.null(x)) {
    if(!is.data.frame(x)) stop("x must be a data frame")
    oldrownames <- rownames(x)
    suppressWarnings(rownames(x) <- 1:nrow(x)) # suppress in case of a tibble
    
    if("data.table" %in% class(x)) {
      y <- x[, c(lon, lat), with = FALSE]
    } else {
      y <- x[c(lon, lat)]  
    }
    
    y$id <- 1:nrow(y)
  }
  
  ## NA action
  
  if(na == "ignore") {
    
    z <- y[eval(is.na(y[[lon]]) | is.na(y[[lat]])),] # eval for data.table
    y <- y[eval(!(is.na(y[[lon]]) | is.na(y[[lat]]))),]
    
  } else if(na == "remove") {
    
    y <- y[eval(!is.na(y[[lon]]) | !is.na(y[[lat]])),]
    y <- y[eval(!is.na(y[[lon]]) | !is.na(y[[lat]])),]
    
    message("Removed rows that contained missing coordinates.")
  } else {
    if(any(c(eval(is.na(y[[lon]])), is.na(y[[lat]])))) {
      stop("lon or lat coordinates contain missing values. Adjust the na argument or take care of the NAs.")
    }
  }
  
  ## Output projection if not defined ---
  
  if(is.null(proj.out)) {
    limits <- c(range(y[[lon]]), range(y[[lat]]))
    shapefile.def <- define_shapefiles(limits)
    proj.out <- sf::st_crs(shapefile_list(shapefile.def$shapefile.name)$crs)
  }
  
  ## Fix the CRS
  
  if("crs" != class(proj.in)) {
    error_test <- quiet(try(sf::st_crs(proj.in), silent = TRUE))
    
    if(class(error_test) == "try-error") {
      stop("Failed to convert the argument proj.in to sf::st_crs object in the transform_coord function. This is likely a bug. If so, please file a bug report on GitHub.")
    } else {
      proj.in <- error_test
    }
  }
  
  if("crs" != class(proj.out)) {
    error_test <- quiet(try(sf::st_crs(proj.out), silent = TRUE))
    
    if(class(error_test) == "try-error") {
      stop("Failed to convert the argument proj.out to sf::st_crs object in the transform_coord function. This is likely a bug. If so, please file a bug report on GitHub.")
    } else {
      proj.out <- error_test
    }
  }
  
  ## Coordinate transformation ---
  
  y <- cbind(
    stats::setNames(
      data.frame(sf::sf_project(from = proj.in, to = proj.out, y[,1:2])), 
      c(lon, lat)),
    id = y$id)
  
  # lon = sample(-30:60, 1e2, replace = TRUE); lat = sample(45:80, 1e2, replace = TRUE); y = data.frame(lon, lat); lon = "lon"; lat = "lat" # <- Debugging code
  
  ## sf version
  # y <- sf::st_as_sf(y, coords = c(lon, lat), 
  #                   crs = as.integer(gsub("\\D", "", proj.in)))
  # 
  # y <- sf::st_coordinates(sf::st_transform(y, proj.out))
  # 
  # colnames(y)[colnames(y) == "X"] <- lon
  # colnames(y)[colnames(y) == "Y"] <- lat
  
  ## sp version 
  # sp::coordinates(y) <- c(lon, lat)
  # sp::proj4string(y) <- if(class(proj.in) == "CRS") {proj.in} else {sp::CRS(proj.in)}
  # 
  # y <- sp::spTransform(y, if(class(proj.out) == "CRS") {proj.out} else {sp::CRS(proj.out)})
  # y <- data.frame(sp::coordinates(y))
  
  ## ----
  
  if(na == "ignore" & nrow(z) > 0) {
    y <- rbind(y, z)
    rownames(y) <- y$id
    y <- y[order(y$id), !colnames(y) %in% "id"]
  } else {
    y <- y[, !colnames(y) %in% "id"]
  }
  
  if(!is.null(new.names)) {
    if(any(length(new.names) != 2, !is.character(new.names))) {
      stop("new.names must be a character vector with length of 2")
    }
    colnames(y) <- new.names
  }
  
  if(verbose) {
    if("input" %in% names(proj.in)) {
      proj.in.msg <- proj.in$input
    } else {
      proj.in.msg <- proj.in
    }
    
    if("input" %in% names(proj.out)) {
      proj.out.msg <- proj.out$input
    } else {
      proj.out.msg <- proj.out
    }
    
    message(paste("projection transformed from", proj.in.msg, "to", proj.out.msg))
  }
  
  # Change column names if proj.in not decimal degrees
  
  if(!sf::st_is_longlat(proj.in) & sf::st_is_longlat(proj.out)) {
    tmp <- colnames(x)
    colnames(x) <- colnames(y)
    colnames(y) <- tmp
  }
  
  # Final modifications ----
  
  if(bind) {
    out <- cbind(x, y)
  } else {
    out <- y
  }
  
  if(exists("oldrownames")) {
    rownames(out) <- oldrownames
    out <- out
  } else {
    out <- out
  }
  
  # Add projection information as attributes
  
  attributes(out)$proj.in <- proj.in
  attributes(out)$proj.out <- proj.out
  
  # Return
  
  out
}

