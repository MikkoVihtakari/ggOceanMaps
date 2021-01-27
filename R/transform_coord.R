#' @title Transform spatial coordinates to another projection
#' @description Transforms spatial coordinates from original projection (decimal degrees assumed) to another projection.
#' @param x Data frame to be transformed. Can be omitted if numeric vectors are assigned to \code{lon} and \code{lat}.
#' @param lon,lat Either a name of the longitude and latitude columns in \code{x} or a numeric vector containing longitude and latitude coordinates. Use \code{NULL} to \link[=guess_coordinate_columns]{guess the longitude and/or latitude columns} in \code{x}.
#' @param new.names Character vector of length 2 specifying the names of transformed longitude and latitude columns, respectively. Alternatively \code{NULL}, which returns column names from \code{x} or "auto", which uses \code{NULL} if \code{bind = FALSE} and \code{c("lon.proj", "lat.proj")} if \code{bind = TRUE}.
#' @param proj.in Original \code{\link[sp:is.projected]{proj4string}} projection. If \code{NULL}, the projection is taken from \code{x}. \code{x} must be a \link[sp:Spatial-class]{Spatial} object in that case.
#' @param proj.out Character. Either \code{NULL}, \code{\link[sp:is.projected]{proj4string}} projection the coordinates should be transformed to or a name of shapefiles in \code{\link{shapefile_list}}. If \code{NULL}, the output projection will be automatically determined from data. This option requires decimal degrees as input option.
#' @param verbose  Logical indicating whether information about the projection should be returned as message. Set to \code{FALSE} to make the function silent.
#' @param bind logical. Should only transformed coordinates be returned (\code{FALSE}, default) or should x be returned with transformed coordinates (\code{TRUE})?
#' @param na character specifying the NA action for missing coordinates. The "ignore" option ignores the coordinates and returns NAs to transformed coordinates. The "remove" option removes missing values from \code{x} returning a message while doing it. Any other character argument will trigger \code{na.fail} stopping the function in case of missing coordinates.
#' @return Returns a data frame with transformed spatial coordinates.
#' @details If \code{x} is specified, the function guesses longitude and latitude columns from \code{x} by default.
#' @family basemap functions
#' @author Mikko Vihtakari
#' @import sp
#' @examples
#' # Coordinates are automatically transformed to the pre-made shapefile 
#' # projections:
#' dt <- data.frame(lon = c(-150, 150), lat = c(60, 90))
#' transform_coord(dt)
#' transform_coord(dt, bind = TRUE)
#' 
#' dt <- data.frame(lon = c(-150, 150), lat = c(20, 50))
#' transform_coord(dt, bind = TRUE) # no transformation required.
#' @export

## Debug data
# lon = NULL; lat = NULL; new.names = "auto"; proj.in = "+init=epsg:4326"; proj.out = NULL; verbose = FALSE; bind = FALSE; na = "ignore"
# lon = "rLon"; lat = "rLat"; new.names = c("rLon.proj", "rLat.proj"); proj.in = "+init=epsg:4326"; proj.out = NULL; verbose = FALSE; bind = TRUE; na = "ignore"
# lon = NULL; lat = NULL; new.names = "auto"; proj.in = "+init=epsg:4326"; proj.out = "+init=epsg:32636"; verbose = FALSE; bind = FALSE; na = "ignore"
transform_coord <- function(x = NULL, lon = NULL, lat = NULL, new.names = "auto", proj.in = "+init=epsg:4326", proj.out = NULL, verbose = FALSE, bind = FALSE, na = "ignore") {
  
  # Checks ----
  
  if(length(new.names) == 1) {
    if(new.names == "auto") {
      new.names <- if(bind) c("lon.proj", "lat.proj") else NULL
    }
  }
  
  if(is.null(proj.out) & !grepl("+proj=longlat", suppressWarnings(sp::CRS(proj.in)))) stop("proj.in has to be decimal degrees when proj.out = NULL.")
  if(!is.null(proj.out)) {
    
    error_test <- quiet(try(match.arg(proj.out, shapefile_list("all")$name), silent = TRUE))
    
    if(class(error_test) != "try-error") {
      proj.out <- paste0("+init=epsg:", shapefile_list(proj.out)$crs)
    }
  }
  
  if(is.null(proj.in)) {
    if(is.null(x)) stop("a spatial object as x is required when proj.in = NULL")
    
    proj.in <- suppressWarnings(sp::proj4string(x))
  }
  
  
  ## Case for defined x and undefined lon or/and lat
  if(!is.null(x) & (is.null(lon) | is.null(lat))) {
    if(all(class(x) != "data.frame")) stop("x argument has to be a data.frame or NULL")
    
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
  if(any(class(x) == "tbl")) {
    x <- as.data.frame(x)
  }
  
  ## Make the data frame ----
  
  if(is.null(x) & (is.numeric(lon) | is.numeric(lat))) {
    if(length(lon) != length(lat)) stop("lat and lon must be of equal length")
    y <- data.frame(lon = lon, lat = lat)
    lon <- "lon"; lat <- "lat"
  }
  
  if(!is.null(x)) {
    if(!is.data.frame(x)) stop("x must be a data frame")
    oldrownames <- rownames(x)
    rownames(x) <- 1:nrow(x)
    y <- x
  }
  
  ## NA action
  
  if(na == "ignore") {
    z <- y[is.na(y[[lon]]) | is.na(y[[lat]]), c(lon, lat)]
    y <- y[!(is.na(y[[lon]]) | is.na(y[[lat]])),]
  } else if(na == "remove") {
    y <- y[!is.na(y[[lon]]) | !is.na(y[[lat]]),]
    message("Removed rows that contained missing coordinates.")
  } else {
    stop("lon or lat coordinates contain missing values. Adjust the na argument or take care of the NAs.")
  }
  
  ## Output projection if not defined ----
  
  if(is.null(proj.out)) {
    limits <- c(range(y[[lon]]), range(y[[lat]]))
    shapefile.def <- define_shapefiles(limits)
    proj.out <- paste0("+init=epsg:",shapefile_list(shapefile.def$shapefile.name)$crs)
  }
  
  ## Coordinate transformation ----
  
  sp::coordinates(y) <- c(lon, lat)
  sp::proj4string(y) <- suppressWarnings(sp::CRS(proj.in)) # original projection
  
  y <- sp::spTransform(y, suppressWarnings(sp::CRS(proj.out)))
  y <- data.frame(sp::coordinates(y))
  
  ## ----
  
  if(na == "ignore" & nrow(z) > 0) {
    y <- rbind(y, z)
    y <- y[order(as.numeric(rownames(y))),]
  }
  
  if(!is.null(new.names)) {
    if(any(length(new.names) != 2, !is.character(new.names))) {
      stop("new.names must be a character vector with length of 2")
    }
    colnames(y) <- new.names
  }
  
  if(verbose) {
    message(paste("projection transformed from", proj.in, "to", proj.out))
  }
  
  # Final modifications ---
  
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

