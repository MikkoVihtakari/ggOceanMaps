#' @title Open Geonorge bathymetry shapefiles
#' @description Opens and formats Geonorge bathymetry shapefiles ready for plotting in ggOceanMaps
#' @param filepath Character string defining the path to the \code{.gml} file. Must contain the file extension.
#' @param layer Character string defining the layer containing depth information. If \code{NULL} assumed to be "dybdeareal".
#' @param verbose Logical indicating whether information the reading process should be returned.
#' @details You can download the bathymetry polygon shapefiles from \href{https://kartkatalog.geonorge.no/metadata/kartverket/dybdedata/2751aacf-5472-4850-a208-3532a51c529a}{Geonorge}. Download the file in \code{GLM} format. 
#' @return An \link[sf:st_polygon]{sf} or \code{sp} object containing the depth polygons. Uses same projection than \code{bathy} (see \code{\link[sf:st_crs]{CRS}}).
#' @author Mikko Vihtakari
#' @family create shapefiles
#' @export

# "https://kartkatalog.geonorge.no/metadata/kartverket/dybdedata/2751aacf-5472-4850-a208-3532a51c529a"
# add proj.out
# filepath <- "~/Downloads/Basisdata_21_Svalbard_25833_Dybdedata_GML.gml"; layer = NULL; verbose = TRUE
geonorge_bathymetry <- function(filepath, layer = NULL, verbose = FALSE) {
  
  ## Define the layer
  
  if(is.null(layer)) {
    lrs <- sf::st_layers(filepath)
    layer <- grep("dybdeareal", lrs$name, ignore.case = TRUE, value = TRUE)
    
    if(length(layer) != 1) stop("Could not guess the layer. Specify the layer argument.")
  }
  
  ## Read
  
  x <- sf::st_read(dsn = filepath, layer = layer, quiet = !verbose)
  
  ## Modify
  
  x <- sf::st_make_valid(x[c("maksimumsdybde")])
  
  names(x)[names(x) == c("maksimumsdybde")] <- "depth"
  
  x$depth <- factor(x$depth, levels = sort(unique(x$depth)))
  
  ## Return
  return(x)
}

# x <- geonorge_bathymetry("~/Downloads/Basisdata_21_Svalbard_25833_Dybdedata_GML.gml")
# bla <- load_map_data(shapefile_list("Svalbard"))
# 
# basemap(limits = c(10.9, 12.65, 78.83, 79.12), shapefiles = list(land = bla$svalbard_land, glacier = NULL, bathy = x), bathymetry = TRUE)
