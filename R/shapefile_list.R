#' @title A list of pre-made shapefiles for \code{basemap}
#' @description Lists available pre-made shapefiles for plotting in the \code{\link{basemap}} function. Gives also instructions how to make custom ones.
#' @param name A character argument giving the name of a pre-made shapefile. Will be partially matched. Use "all" to list all available ones.
#' @param get.data Logical indicating whether spatial data should be returned instead of names of spatial data objects.
#' @details Custom shapefiles for \code{\link{basemap}} should be defined as lists with (at least) following names (everything should be provided as characters):
#' \itemize{
#' \item \strong{land} Object name of the \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} containing land. Required.
#' \item \strong{glacier} Object name of the \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} containing glaciers. Use \code{NULL} if glaciers are not needed.
#' \item \strong{bathy} Object name of the \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} containing bathymetry contours. Use \code{NULL} if bathymetry is not needed.
#' }
#'
#' All linked spatial data objects must be in same projection. Pre-made shapefiles contain additional elements that are used in the \code{\link{basemap}} function, but not required for custom shapefile datasets.
#'
#' @return Returns a data frame of provided pre-made shapefiles, if \code{name = "all"}. Returns a shapefile list containing the information for a particular map otherwise.
#' @family basemap functions
#' @keywords shapefiles
#' @author Mikko Vihtakari
#' @examples
#' shapefile_list("all")
#' shapefile_list("Arctic") # partial matching
#' @export

shapefile_list <- function(name, get.data = FALSE) {

  # List of alternatives

  alternatives <- list(
    list(name = "ArcticStereographic", 
         land = "ggOceanMapsData::arctic_land", 
         glacier = "ggOceanMapsData::arctic_glacier",
         bathy = "ggOceanMapsData::arctic_bathy",
         crs = 3995,
         limits = 30,
         path = NA),
    list(name = "AntarcticStereographic", 
         land = "ggOceanMapsData::antarctic_land",
         glacier = "ggOceanMapsData::antarctic_glacier",
         bathy = "ggOceanMapsData::antarctic_bathy",
         crs = 3031,
         limits = -35,
         path = NA),
    list(name = "DecimalDegree", 
         land = "ggOceanMapsData::dd_land",
         glacier = "ggOceanMapsData::dd_glacier",
         bathy = "ggOceanMapsData::dd_bathy",
         crs = 4326,
         limits = c(-180, 180, -90, 90),
         path = NA),
    list(name = "Svalbard",
         land = "svalbard_land",
         glacier = "svalbard_glacier",
         bathy = "svalbard_bathy",
         crs = 32633,
         limits = c(402204.7, 845943.9, 8253526.1, 8978517.5),
         path = "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/svalbard.rda"),
    list(name = "BarentsSea",
         land = "barentssea_land",
         glacier = "barentssea_glacier",
         bathy = "barentssea_bathy",
         crs = 32636,
         limits = c(-400000, 1300000, 7400000, 9350000),
         path = "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/barentssea.rda"),
    list(name = "IBCAO",
         land = "ggOceanMapsData::arctic_land", 
         glacier = "ggOceanMapsData::arctic_glacier",
         bathy = "ibcao_bathy",
         crs = 3995,
         limits = c(-2879810, 2700490, -2879810, 2879810),
         path = "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/ibcao_bathy.rda"),
    list(name = "GEBCO",
         land = "ggOceanMapsData::arctic_land", 
         glacier = "ggOceanMapsData::arctic_glacier",
         bathy = "gebco_bathy",
         crs = 3995,
         limits = 30,
         path = "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/gebco_bathy.rda"),
    list(name = "EMODnet",
         land = "emodnet_land", 
         glacier = NA,
         bathy = "emodnet_bathy",
         crs = 3575,
         limits = c(-3e5, -1e5, -3.1e6, -2.9e6),
         path = "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/emodnet.rda")
  )

  names(alternatives) <- sapply(alternatives, function(k) k$name)

  # Return

  if(name == "all") {
    
    out <- do.call(rbind, lapply(alternatives, function(k) data.frame(k, stringsAsFactors = FALSE)))
    rownames(out) <- 1:nrow(out)
    
  } else  {
    out <- alternatives[[match.arg(name, names(alternatives))]] # Allows partial matching (typos likely in name)

    if(get.data) {
      out[c("land", "glacier", "bathy")] <- lapply(out[c("land", "glacier", "bathy")], function(k) get(k))
    }
  }
  
  out
}

