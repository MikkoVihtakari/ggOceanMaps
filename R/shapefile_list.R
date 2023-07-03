#' @title A list of pre-made shapefiles for \code{basemap}
#' @description Lists available pre-made shapefiles for plotting in the \code{\link{basemap}} function. Gives also instructions how to make custom ones.
#' @param name A character argument giving the name of a pre-made shapefile. Will be partially matched. Use "all" to list all available ones.
#' @param get.data Logical indicating whether spatial data should be returned instead of names of spatial data objects.
#' @details Custom shapefiles for \code{\link{basemap}} should be defined as lists with (at least) following names (everything should be provided as characters):
#' \itemize{
#' \item \strong{land} Name of the object containing land polygons. Required.
#' \item \strong{glacier} Name of the object containing glacier polygons. Use \code{NULL} if glaciers are not needed.
#' \item \strong{bathy} Name of the object containing land polygons. Use \code{NULL} if bathymetry is not needed.
#' }
#'
#' All linked spatial data objects must be in same projection. High-resolution pre-made data are still under development and may not be available. Pre-made shapefiles contain additional elements that are used in the \code{\link{basemap}} function, but not required for custom shapefile datasets.
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
         land = file.path(options("ggOceanMaps.datapath"), "arctic_land"), 
         glacier = file.path(options("ggOceanMaps.datapath"), "arctic_glacier"),
         bathy = c(
           "raster_binned" = "dd_rbathy",
           "raster_continuous" = file.path(options("ggOceanMaps.datapath"), "dd_rbathy_cont"),
           "raster_user" = getOption("ggOceanMaps.userpath"),
           "vector" = file.path(options("ggOceanMaps.datapath"), "arctic_bathy")),
         crs = 3995,
         limits = 30,
         path = c("ggOceanMapsLargeData" = 
                    "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/")
    ),
    list(name = "AntarcticStereographic", 
         land = file.path(options("ggOceanMaps.datapath"), "antarctic_land"),
         glacier = file.path(options("ggOceanMaps.datapath"), "antarctic_glacier"),
         bathy = c(
           "raster_binned" = "dd_rbathy",
           "raster_continuous" = file.path(options("ggOceanMaps.datapath"), "dd_rbathy_cont"),
           "raster_user" = getOption("ggOceanMaps.userpath"),
           "vector" = file.path(options("ggOceanMaps.datapath"), "antarctic_bathy")),
         crs = 3031,
         limits = -35,
         path = c("ggOceanMapsLargeData" = 
                    "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/")
    ),
    list(name = "DecimalDegree", 
         land = "dd_land",
         glacier = file.path(options("ggOceanMaps.datapath"), "dd_glacier"),
         bathy = c(
           "raster_binned" = "dd_rbathy",
           "raster_continuous" = file.path(options("ggOceanMaps.datapath"), "dd_rbathy_cont"),
           "raster_user" = getOption("ggOceanMaps.userpath"),
           "vector" = file.path(options("ggOceanMaps.datapath"), "dd_bathy")),
         crs = 4326,
         limits = c(-180, 180, -90, 90),
         path = c("ggOceanMapsLargeData" = 
                    "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/")
    ),
    list(name = "Svalbard",
         land = file.path(options("ggOceanMaps.datapath"), "svalbard_land"),
         glacier = file.path(options("ggOceanMaps.datapath"), "svalbard_glacier"),
         bathy = c(
           "raster_binned" = "dd_rbathy",
           "raster_continuous" = file.path(options("ggOceanMaps.datapath"), "dd_rbathy_cont"),
           "raster_user" = getOption("ggOceanMaps.userpath"),
           "vector" = file.path(options("ggOceanMaps.datapath"), "svalbard_bathy")),
         crs = 32633,
         limits = c(402204.7, 845943.9, 8253526.1, 8978517.5),
         path = c("ggOceanMapsLargeData" = 
                    "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/")
    ),
    list(name = "BarentsSea",
         land = file.path(options("ggOceanMaps.datapath"), "barentssea_land"),
         glacier = file.path(options("ggOceanMaps.datapath"), "barentssea_glacier"),
         bathy = c(
           "raster_binned" = "barentssea_rbathy",
           "raster_continuous" = file.path(options("ggOceanMaps.datapath"), "dd_rbathy_cont"),
           "raster_user" = getOption("ggOceanMaps.userpath"),
           "vector" = file.path(options("ggOceanMaps.datapath"), "barentssea_bathy")),
         crs = 32636,
         limits = c(-400000, 1300000, 7400000, 9350000),
         path = "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/barentssea.rda"
    )#,
    # list(name = "IBCAO",
    #      land = file.path(options("ggOceanMaps.datapath"), "ibcao_land"), 
    #      glacier = file.path(options("ggOceanMaps.datapath"), "ibcao_glacier"),
    #      bathy = c(
    #        "raster_binned" = "ibcao_rbathy",
    #        "raster_continuous" = file.path(options("ggOceanMaps.datapath"), "ibcao_rbathy_cont"),
    #        "raster_user" = getOption("ggOceanMaps.userpath"),
    #        "vector" = file.path(options("ggOceanMaps.datapath"), "ibcao_bathy")),
    #      crs = 3995,
    #      limits = c(-2879810, 2700490, -2879810, 2879810),
    #      path = "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/ibcao_bathy.rda"
    # ),
    # list(name = "GEBCO",
    #      land = file.path(options("ggOceanMaps.datapath"), "gebco_land"), 
    #      glacier = file.path(options("ggOceanMaps.datapath"), "gebco_glacier"),
    #      bathy = c(
    #        "raster_binned" = "ibcao_rbathy",
    #        "raster_continuous" = file.path(options("ggOceanMaps.datapath"), "gebco_rbathy_cont"),
    #        "raster_user" = getOption("ggOceanMaps.userpath"),
    #        "vector" = file.path(options("ggOceanMaps.datapath"), "gebco_bathy")),
    #      crs = 3995,
    #      limits = 30,
    #      path = "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/gebco_bathy.rda"
    # ),
    # list(name = "EMODnet",
    #      land = file.path(options("ggOceanMaps.datapath"), "emodnet_land"), 
    #      glacier = NA,
    #      bathy = c(
    #        "raster_binned" = "emodnet_rbathy",
    #        "raster_continuous" = file.path(options("ggOceanMaps.datapath"), "emodnet_rbathy_cont"),
    #        "raster_user" = getOption("ggOceanMaps.userpath"),
    #        "vector" = file.path(options("ggOceanMaps.datapath"), "emodnet_bathy")),
    #      crs = 3575,
    #      limits = c(-3e5, -1e5, -3.1e6, -2.9e6),
    #      path = "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/emodnet.rda"
    # )
  )
  
  names(alternatives) <- sapply(alternatives, function(k) k$name)
  
  # Return
  
  if(name == "all") {
    
    out <- 
      do.call(rbind, lapply(alternatives, function(k) {
        k$bathy <- paste(k$bathy, collapse = "|")
        k$limits <- paste0("c(",paste(k$limits, collapse = ", "), ")")
        data.frame(k, stringsAsFactors = FALSE)
      })
      )
    
    rownames(out) <- 1:nrow(out)
    
  } else  {
    out <- alternatives[[match.arg(name, names(alternatives))]] # Allows partial matching (typos likely in name)
    
    if(get.data) {
      out[c("land", "glacier", "bathy")] <- lapply(out[c("land", "glacier", "bathy")], function(k) get(k))
    }
  }
  
  out
}

