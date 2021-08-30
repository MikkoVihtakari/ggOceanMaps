#' @title Calculate distance to the closest land for coordinates in a data frame
#' @description Calculates the closest distance to land for coordinates in a data frame
#' @param data Data.frame containing geographic coordinates
#' @param lon,lat Either the names of the longitude and latitude columns in \code{data} or \code{NULL} to \link[=guess_coordinate_columns]{guess the longitude and/or latitude columns} in \code{data}.
#' @param proj.in \code{\link[sp:is.projected]{proj4string}} projection argument for the coordinates in \code{data}.
#' @param shapefile Land shape to which distances should be calculated. Either a character argument referring to a name of pre-made shapefiles in \code{\link{shapefile_list}}, a single \link[sp]{SpatialPolygons} object or \code{NULL} to enable automatic definition of the land shapes based on \code{data}.
#' @param bind Logical indicating whether \code{x} should be returned with the distances (\code{TRUE}, default) or should the distances be returned as vector (\code{FALSE}).
#' @param dist.col The name of the distance column, if \code{bind = TRUE}. Defaults to "dist".
#' @param binary Logical indicating whether binary (TRUE = the position is in the ocean, FALSE = the position is on land) should be returned instead of distances. Speeds up the function considerably.
#' @param cores Integer value defining how many cores should be used in the distance calculations. Parallelization speeds up the function (see \code{parallel::mclapply}), but naturally eats up computer resources during the calculation. Set to 1 to remove parallelization.
#' @param verbose  Logical indicating whether information about the process should be returned as messages. Set to \code{FALSE} to make the function silent.
#' @details The function calculates distances using projected coordinates and the \code{rgeos::gDistance} function. These distances do not consider the curvature of the Earth unless the projection of the used land shape does so (check out geosphere::dist2Line and \href{https://stackoverflow.com/a/51842756/1082004}{this SO solution if you want exact distances}). The function is fairly slow for large datasets. If you only want to use the function to remove (wrong) observations reported on land, set the \code{binary} argument to \code{TRUE}. This speeds up the calculations considerably.
#'
#' The \code{dist2land} function offers parallel processing, which speeds up the calculations for large datasets. Parallel processing has not been tested under Windows yet and may not work.
#' @return Returns a vector if \code{bind = FALSE}, otherwise a data frame. The distances are given in a new column defined by the \code{dist.col} argument. The distances are \strong{kilometers} if \code{binary = FALSE}, otherwise logical (TRUE = the position is in the ocean, FALSE = the position is on land).
#' @import sp
#' @importFrom rgeos createSPComment gDistance
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom parallel makeCluster parLapply stopCluster mclapply
#' @author Mikko Vihtakari
#' @examples
#' \donttest{
#' # Simple example:
#' dt <- data.frame(lon = seq(-20, 80, length.out = 41), lat = 50:90)
#' dt <- dist2land(dt, cores = 1)
#' qmap(dt, color = ldist) + scale_color_viridis_c()
#'
#' # No premade shapefiles for datasets covering the entire globe
#' data.frame(lon = -20:20, lat = seq(-90, 90, length.out = 41))
#' dist2land(dt, cores = 1) # wrong!
#' }
#' \dontrun{
#' dt <- data.frame(lon = seq(-179, 179, length.out = 1000), lat = rep(60, 1000))
#' # The distance calculation is slow for large datasets
#' system.time(dist2land(dt))
#' #> user  system elapsed
#' #> 0.073   0.041   5.627
#'
#' # The parallel processing speeds it up
#' system.time(dist2land(dt, cores = 1))
#' #> user  system elapsed
#' #> 19.719   1.237  20.894
#'
#' # binary = TRUE further speeds the function up
#' system.time(dist2land(dt, binary = TRUE))
#' #> user  system elapsed
#' #> 1.624   0.041   1.680
#' }
#' @export

## Test parameters
# lon = NULL; lat = NULL; shapefile = NULL; proj.in = convert_crs(4326); bind = TRUE; dist.col = "ldist"; binary = FALSE; geodesic.distances = FALSE; verbose = TRUE; cores = getCores()

dist2land <- function(data, lon = NULL, lat = NULL, shapefile = NULL, proj.in = convert_crs(4326), bind = TRUE, dist.col = "ldist", binary = FALSE, cores = getCores(), verbose = TRUE) {

  ## Case for defined x and undefined lon or/and lat ####

  if(is.null(lon) | is.null(lat)) {
    if(all(class(data) != "data.frame")) stop("x argument has to be a data.frame")

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

  ## Land shape ###

  if(!is.null(shapefile)) {

    error_test <- quiet(try(match.arg(shapefile, shapefile_list("all")$name), silent = TRUE))

    if(class(error_test) != "try-error") {
      shapefile <- shapefile_list(shapefile)
      if(verbose) message(paste("Using", shapefile$name, "as land shapes."))
      land <- eval(parse(text = shapefile$land))
    } else {
      if(!class(shapefile) %in% c("SpatialPolygonsDataFrame", "SpatialPolygons")) stop("The shapefile must either be matchable string to shapefile_list or a SpatialPolygons object.")
      if(verbose) message("Using custom land shapes.")
      land <- shapefile
    }
  } else {

    ddLimits <- auto_limits(data = x, lon = "lon", lat = "lat", proj.in = proj.in, verbose = FALSE)$ddLimits

    shapefile.def <- define_shapefiles(ddLimits)
    shapefile <- shapefile_list(shapefile.def$shapefile.name)
    if(verbose) message(paste("Using", shapefile$name, "as land shapes."))

    land <- eval(parse(text = shapefile$land))
  }

  land <- rgeos::createSPComment(land)

  ## Coordinate points ###

  sp::coordinates(x) <- c("lon", "lat")
  sp::proj4string(x) <- sp::CRS(proj.in)
  x <- suppressWarnings(sp::spTransform(x, sp::CRS(suppressWarnings(sp::proj4string(land)))))
  if(!suppressWarnings(sp::identicalCRS(land, x))) stop("Cannot convert projections correctly.")

  ############################
  ## Distance calculation ####

  if(binary) {## Binary positions

    if(verbose) message("Calculating binary positions...")
    tmp <- suppressWarnings(unname(is.na(sp::over(x, land)[1])))

  } else { ## gDistance

    if(cores == 1) {
      if(verbose) message("Calculating distances without parallel processing...")
      pb <- utils::txtProgressBar(min = 0, max = length(x), style = 3)
      tmp <- sapply(1:length(x), function(i)  {
        utils::setTxtProgressBar(pb, i)
        return(suppressWarnings(rgeos::gDistance(x[i], land)/1000))
      })
    }
    # Run in parallel on Windows and other platforms:
    else {
      # Do not use more cores than the number of files:
      cores <- min(length(x), cores)

      if(verbose) message("Calculating distances with parallel processing...")

      # On Windows run special args to speed up:
      if(.Platform$OS.type == "windows") {
        cl <- parallel::makeCluster(cores, rscript_args = c("--no-init-file", "--no-site-file", "--no-environ"))
        out <- parallel::parLapply(cl, 1:length(x), function(i) suppressWarnings(rgeos::gDistance(x[i], land)/1000))
        parallel::stopCluster(cl)
      }
      else {
        tmp <- unlist(parallel::mclapply(1:length(x), function(i) {
          suppressWarnings(rgeos::gDistance(x[i], land)/1000)
        }, mc.cores = cores))
      }
    }
  }

  ## Return

  if(contains.nas) {
    na.rows[!na.rows] <- tmp
    na.rows[na.rows == 1] <- NA
    tmp <- na.rows
  }

  if(bind) {
    data[[dist.col]] <- tmp
    data
  } else {
    tmp
  }

}


