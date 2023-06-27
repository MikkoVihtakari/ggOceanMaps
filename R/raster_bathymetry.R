#' @title Simplify a bathymetry raster ready for vectorization
#' @description Simplifies bathymetry raster ready for the \code{\link{vector_bathymetry}} function. Warning: processing may take a long time if the bathymetry raster is large.
#' @param bathy A \link[stars:read_stars]{stars} object or a string giving the path to a bathymetry NetCDF or grd file
#' @param depths Numeric vector giving the cut points for depth contours (see \code{\link[base]{cut}}). Use 0 as first depth if you want to vectorise land from \code{bathy} during the \code{\link{vector_bathymetry}} step.
#' @param proj.out A character string specifying the \link[sf:st_crs]{coordinate reference system} (CRS)  argument for the output. See \code{\link[sf]{st_crs}} and \href{https://proj.org/}{proj.org}. If \code{NULL}, the projection is retrieved from \code{bathy} and the output will not be reprojected saving processing time (since \code{proj.out} and \code{proj.bathy} would match. 
#' @param proj.bathy A character string specifying the \code{\link[sf:st_crs]{CRS}} for the input (\code{bathy}). Only required if \code{bathy} lacks CRS information. If \code{NULL}, \code{"EPSG:4326"} is assumed.
#' @param boundary A \link[sf]{st_polygon} object, text string defining the file path to a spatial polygon, \link[sf:st_bbox]{bounding box}, or a numeric vector of length 4 giving the boundaries for which \code{bathy} should be cut to. Should be given as \strong{decimal degrees}. If unnamed numeric vector, the first element defines the minimum longitude, the second element the maximum longitude, the third element the minimum latitude and the fourth element the maximum latitude of the bounding box. You can also use the sf bounding box format as named vector. Use \code{NULL} not to cut \code{bathy}.
#' @param downsample An integer defining how many rows in \code{bathy} should be skipped to reduce the size (and resolution). 1 skips every second row, 2 every second and third. See \code{\link[stars]{st_downsample}}. Set to \code{NULL} (default) to skip downsampling.
#' @param verbose Logical indicating whether information about guessed projection should be returned as message. Set to \code{FALSE} to make the function silent.
#' @details You can use \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/}{GEBCO}, \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/arctic_ocean/}{IBCAO}, \href{https://www.ngdc.noaa.gov/mgg/global/}{ETOPO1} bathymetry grids downloaded from respective sources as the \code{bathy} argument. The bathymetry grids read from files must be in any format read by \code{\link[stars]{read_stars}}. Alternatively use the \code{marmap::getNOAA.bathy} function to download ETOPO1 bathymetry and convert it to a raster object using the \code{marmap::as.raster} function.
#'
#' Note that the size of the output is heavily influenced by the number of depth contours (\code{depths}) as well as the resolution of \code{bathy} and choice of \code{downsample}. To make the \code{\link{vector_bathymetry}} function and consequent plotting faster, limiting the details of the bathymetry raster may be desirable.
#' @return A list with a \link[stars:read_stars]{stars} object the containing projected bathymetry defined by the \code{proj.out} argument and a data frame of depth intervals.
#' @references GEBCO Compilation Group (2019) GEBCO 2019 15-arcsecond grid (doi:10.5285/836f016a-33be-6ddc-e053-6c86abc0788e). URL: \url{https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2019/gebco_2019_info.html}.
#' ETOPO1 1 Arc-Minute Global Relief Model. URL: \url{https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/docs/ETOPO1.pdf}.
#' @author Mikko Vihtakari
#' @family create shapefiles
#' @export

# bathy = "/Users/a22357/Downloads/ETOPO_2022_v1_60s_N90W180_surface.nc"
# depths = c(0, 50, 300, 500, 1000, 1500, 2000, 4000, 6000, 10000); proj.out = 4326; file.name = NULL; boundary = c(-180, 180, -90, 90); aggregation.factor = 6
# proj.out = shapefile_list("Barents")$crs
# bathy = file.path(etopoPath, "ETOPO1_Ice_g_gmt4.grd"); depths = c(0, 50, 300, 500, 1000, 1500, 2000, 4000, 6000, 10000); proj.out = convert_crs("3996"); proj.bathy = convert_crs("3996"), file.name = NULL; boundary = c(-180.0083, 180.0083, -90, 90); aggregation.factor = 6
# bathy = file.path(etopoPath, "ETOPO1_Ice_g_gmt4.grd"); depths = c(50, 300, 500, 1000, 1500, 2000, 4000, 6000, 10000); proj.out = arcticCRS; boundary = c(-180.0083, 180.0083, 30, 90); aggregation.factor = 2; file.name = NULL; verbose = TRUE
# proj.out = NULL; proj.bathy = NULL; boundary = NULL; downsample = NULL; verbose = TRUE
raster_bathymetry <- function(bathy, depths, proj.out = NULL, proj.bathy = NULL, boundary = NULL, downsample = NULL, verbose = TRUE) {
  
  # Progress bar ####
  
  pb <- utils::txtProgressBar(min = 0, max = 7, initial = 0, style = 3)
  
  ## General checks ####
  
  ### Bathy argument
  
  if(!inherits(bathy, "RasterLayer")) {
    if(!file.exists(bathy)) stop("Bathy raster file not found. Check the path in the bathy argument.")
  }
  
  ### The depths argument
  
  if(!(is.vector(depths) & inherits(depths, c("numeric", "integer")))) {
    stop("The depths parameter has to be a numeric or integer vector.")
  }
  
  ### The boundary argument
  
  if(!is.null(boundary)) {
    
    #### If spatialpolygons
    if(any(grepl("spatialpolygons|sf|sfc", class(boundary), ignore.case = TRUE))) {
      
      if(any(grepl("spatialpolygons", class(boundary), ignore.case = TRUE))) {
        boundary <- sf::st_as_sf(boundary)
      }
      
      if(is.na(sf::st_crs(boundary))) {
        stop("boundary does not contain argument.")
      } else if(!sf::st_is_longlat(boundary)) {
        stop("boundary has to be defined as decimal degrees")
      }
      
      #### If file
    } else if(is.character(boundary) & length(boundary) == 1) {
      if(!file.exists(boundary)) stop("Boundary shapefile not found. Check your path")
      
      boundary <- sf::st_read(boundary, quiet = !verbose)
      
      if(is.na(sf::st_crs(boundary))) {
        stop("boundary does not have a defined CRS.")
      } else if(!sf::st_is_longlat(boundary)) {
        stop("boundary has to be defined as decimal degrees")
      }
      
    } else if(!(is.vector(boundary) & inherits(boundary, c("numeric", "integer")) &
                length(boundary) == 4)) {
      stop("The boundary parameter has to be a numeric/integer vector of length 4 giving the decimal degree longitude and latitude limits for the strata region OR a character argument giving the location of the shapefile polygon.")
    } else if(length(boundary) == 4 & inherits(boundary, c("numeric", "integer"))) {
      if(is.null(names(boundary))) names(boundary) <- c("xmin", "xmax", "ymin", "ymax")
      
      boundary <- sf::st_bbox(boundary, crs = 4326)
    }
  }
  
  utils::setTxtProgressBar(pb, 1)
  
  ## Open raster ###
  
  if(inherits(bathy, "stars")) {
    ras <- bathy
  } else {
    ras <- stars::read_stars(bathy, quiet = !verbose)
    # ras <- terra::rast(bathy)
    # ras <- raster::raster(bathy)
  }
  
  utils::setTxtProgressBar(pb, 2)
  
  if(is.null(proj.bathy)) {
    if(is.na(sf::st_crs(ras))) {
      proj.bathy <- 4326
    } else {
      proj.bathy <- sf::st_crs(ras)
    }
  }
  
  if(is.na(sf::st_crs(ras))) {
    if(verbose) message(paste0("bathy does not contain coordinate reference information. Using ", proj.bathy, ". Adjust this setting by changing the proj.bathy argument"))
    ras <- sf::st_set_crs(ras, proj.bathy)
  }
  
  if(!is.null(boundary)) {
    
    if(inherits(boundary, c("sf", "sfc", "bbox"))) {
      ras <- ras[sf::st_bbox(boundary)]
      # ras <- raster::crop(ras, raster::extent(boundary))
      # ras <- terra::crop(ras, boundary)
    } else {
      stop("boundary must be an sf object at this stage")
    }
  }
  
  utils::setTxtProgressBar(pb, 3)
  
  ## Set proj.out (if not set)
  
  if(is.null(proj.out) & !is.na(sf::st_crs(ras))) {
    proj.out <- sf::st_crs(ras)
  }
  
  ## Downsample (reduce size) ####
  
  if(!is.null(downsample)) {
    if(verbose) {
      ras <- stars::st_downsample(ras, n = downsample)
    } else {
      ras <- suppressMessages(stars::st_downsample(ras, n = downsample))
    }
  }
  
  utils::setTxtProgressBar(pb, 4)
  
  ## Project the raster ####
  
  if(sf::st_crs(ras) != sf::st_crs(proj.out)) {
    ras <- sf::st_transform(ras, sf::st_crs(proj.out))
    # ras <- raster::projectRaster(from = ras, crs = raster::crs(proj.out))
    # ras <- terra::project(ras, sf::st_crs(proj.out)$input)
  }
  
  utils::setTxtProgressBar(pb, 5)
  
  ## Read to memory if not done already
  
  if(inherits(ras, "stars_proxy")){
    ras <- stars::st_as_stars(ras)
  }
  
  utils::setTxtProgressBar(pb, 6)
  
  ## Reclassify raster ####
  
  if(all(depths >= 0)) depths <- sort(-1 * depths)
  
  depths <- sort(unique(c(-Inf, depths, 0, Inf)))
  
  cut_int <- paste(abs(depths[-1]), abs(depths[-length(depths)]), sep = "-")
  
  cut_df <- data.frame(
    from = depths[-length(depths)],
    to = depths[-1],
    average = sapply(strsplit(cut_int, "-"), function(k) mean(as.numeric(k))),
    interval = cut_int,
    stringsAsFactors = FALSE
  )
  
  cut_df[nrow(cut_df), "average"] <- NA
  
  # cut_matrix <- as.matrix(cut_df[-ncol(cut_df)])
  
  r <- cut(ras, c(cut_df$from, Inf), labels = cut_df$interval)
  
  # r <- raster::reclassify(
  #   ras, 
  #   rcl = cut_matrix,
  #   right = NA, 
  #   filename = ifelse(is.null(file.name), '', paste0(file.name, ".grd"))
  # )
  
  utils::setTxtProgressBar(pb, 7)
  
  ## Return
  
  out <- list(raster = r, depth.invervals = cut_df)
  
  class(out) <- "bathyRaster"
  
  out
}
