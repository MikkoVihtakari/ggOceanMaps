#' @title Simplify a bathymetry raster ready to be vectorized
#' @description Simplifies bathymetry raster ready for the \code{\link{vector_bathymetry}} function. Warning: processing may take a long time if the bathymetry raster is large.
#' @param bathy A \link[raster]{raster} object or a string giving the path to a bathymetry NetCDF or grd file
#' @param depths Numeric vector giving the cut points for depth contours (see \code{\link[base]{cut}}.
#' @param proj.out A character string specifying the PROJ.4 projection arguments for the output. See \code{\link[sp:CRS-class]{CRS}} and \href{https://proj.org/}{proj.org}. 
#' @param boundary A \link[sp]{SpatialPolygons}(DataFrame) object, text string defining the file path to a spatial polygon, or a numeric vector of length 4 giving the boundaries for which \code{bathy} should be cut to. Should be given as \strong{decimal degrees}. If numeric vector, the first element defines the minimum longitude, the second element the maximum longitude, the third element the minimum latitude and the fourth element the maximum latitude of the bounding box. Use \code{NULL} not to cut \code{bathy}.
#' @param file.name A character string specifying the file path \strong{without extension} where the output should be saved. If \code{NULL} a temporary file will be used. See \code{\link[raster]{writeRaster}}.
#' @param aggregation.factor An integer defining the \code{fact} argument from the \code{\link[raster]{aggregate}} function. Set to \code{NA} to ignore aggregation.
#' @details You can use \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/}{GEBCO} or \href{https://www.ngdc.noaa.gov/mgg/global/}{ETOPO1} bathymetry grids downloaded from respective sources as the \code{bathy} argument. The bathymetry grids read from files must be in NetCDF/grd format and defined using decimal degrees. Alternatively use the \code{marmap::getNOAA.bathy} function to download ETOPO1 bathymetry and convert it to a raster object using the \code{marmap::as.raster} function.
#'
#' Note that the size of the output is heavily influenced by the number of depth contours (\code{depths}) as well as the resolution of \code{bathy} and choise of \code{aggregation.factor}. To make the \code{\link{vector_bathymetry}} function and consequent plotting faster, limiting the details of the bathymetry raster may be desireable.
#' @return A list with \link[raster]{raster} object containing projected bathymetry defined by the \code{proj.out} argument and a data frame of depth intervals.
#' @references GEBCO Compilation Group (2019) GEBCO 2019 15-arcsecond grid (doi:10.5285/836f016a-33be-6ddc-e053-6c86abc0788e). URL: \url{https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2019/gebco_2019_info.html}.
#' ETOPO1 1 Arc-Minute Global Relief Model. URL: \url{https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/docs/ETOPO1.pdf}.
#' @import sp
#' @importFrom rgdal readOGR
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @rawNamespace import(raster, except = shift)
#' @author Mikko Vihtakari
#' @family create shapefiles
#' @export

# bathy = "../../../GIS/ETOPO bathymetry/ETOPO1_Bed_g_gmt4.grd"; boundary = c(-180.0083, 180.0083, -80, -55)
# bathy = "../../../GIS/GEBCO bathymetry/GEBCO_2014/GEBCO_2014_1D.nc"; depths = c(50, 100, 200, 300, 400, 500, 700, 1000, 1500, 2000, 3000, 4000, 5000, 6000, 10000); boundary = c(-180.00083, 180.00083, 30, 90.00083); proj.out = map_projection("panarctic"); file.name = "bathy"

# bathy = "../../../../GIS/ETOPO bathymetry/ETOPO1_Ice_g_gmt4.grd"; depths = c(50, 300, 500, 1000, 1500, 2000, 4000, 6000, 10000); proj.out = "+init=epsg:3995"; boundary = c(-180.0083, 180.0083, 90, -30); file.name = NULL; aggregation.factor = 2

raster_bathymetry <- function(bathy, depths, proj.out, boundary = NULL, file.name = NULL, aggregation.factor = NA) {

  # Progress bar ####

  pb <- utils::txtProgressBar(min = 0, max = 6, initial = 0, style = 3)

  ## General checks ####

  ### Bathy argument

  if(class(bathy) != "RasterLayer") {
    if(!file.exists(bathy)) stop("Bathy raster file not found. Check the path in the bathy argument.")
  }

  ### The depths argument

  if(!(is.vector(depths) & class(depths) %in% c("numeric", "integer"))) {
    stop("The depths parameter has to be a numeric or integer vector.")
  }

  ### The boundary argument

  if(!is.null(boundary)) {

    #### If spatialpolygons
    if(grepl("spatialpolygons", class(boundary), ignore.case = TRUE)) {

      if(is.null(sp::proj4string(boundary))) {
        stop("boundary misses proj4string argument.")
      } else if(!grepl("+proj=longlat", sp::proj4string(boundary))) {
        stop("boundary has to be defined as decimal degrees")
      }

      #### If file
    } else if(class(boundary) == "character" & length(boundary) == 1) {
      if(!file.exists(boundary)) stop("Boundary shapefile not found. Check your path")

      boundary <- rgdal::readOGR(boundary, verbose = FALSE)

      if(is.null(sp::proj4string(boundary))) {
        stop("boundary misses proj4string argument.")
      } else if(!grepl("\\+proj=longlat", sp::proj4string(boundary))) {
        stop("boundary has to be defined as decimal degrees")
      }

    } else if(!(is.vector(boundary) & class(boundary) %in% c("numeric", "integer") & length(boundary) == 4)) {
      stop("The boundary parameter has to be a numeric/integer vector of length 4 giving the decimal degree longitude and latitude limits for the strata region OR a character argument giving the location of the shapefile polygon.")
    }
  }

  utils::setTxtProgressBar(pb, 1)

  ## Open raster ###

  if(class(bathy) == "RasterLayer") {
    ras <- bathy
  } else {
    ras <- raster::raster(bathy)
  }

  utils::setTxtProgressBar(pb, 2)

  if(is.na(sp::proj4string(ras))) {
    message("bathy does not contain coordinate reference information. Assuming decimal degree projection")
    sp::proj4string(ras) <- "+init=epsg:4326"
  }

  if(!grepl("\\+proj=longlat", sp::proj4string(ras))) stop("bathy has to be in decimal degree projection. Use '+init=epsg:4326'")

  if(!is.null(boundary)) {

    ras <- raster::crop(ras, raster::extent(boundary))

    if(grepl("spatialpolygons", class(boundary), ignore.case = TRUE)) {
      ras <- raster::mask(ras, boundary)
    }
  }

  utils::setTxtProgressBar(pb, 3)

  ## Aggregate (reduce size) ####

  if(!is.na(aggregation.factor)) ras <- raster::aggregate(ras, fact = aggregation.factor)

  utils::setTxtProgressBar(pb, 4)

  ## Project the raster ####

  ras <- raster::projectRaster(from = ras, crs = sp::CRS(proj.out))

  utils::setTxtProgressBar(pb, 5)

  ## Reclassify raster ####

  if(all(depths >= 0)) depths <- sort(-1 * depths)

  depths <- sort(unique(c(-Inf, depths, 0, Inf)))

  cut_int <- paste(abs(depths[-1]), abs(depths[-length(depths)]), sep = "-")

  cut_df <- data.frame(from = depths[-length(depths)],
                       to = depths[-1],
                       average = sapply(strsplit(cut_int, "-"), function(k) mean(as.numeric(k))),
                       interval = cut_int,
                       stringsAsFactors = FALSE)

  cut_df[nrow(cut_df), "average"] <- NA

  cut_matrix <- as.matrix(cut_df[-ncol(cut_df)])

  r <- raster::reclassify(ras, rcl = cut_matrix, right = NA, filename = ifelse(is.null(file.name), '', paste0(file.name, ".grd")))

  utils::setTxtProgressBar(pb, 6)

  ## Return

  out <- list(raster = r, depth.invervals = cut_df)

  class(out) <- "bathyRaster"

  out
}
