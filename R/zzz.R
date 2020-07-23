#' @importFrom utils install.packages
.onLoad <- function(libname, pkgname) {
  
  # Set repo for the data package
  
  repos <- getOption("repos")
  repos["ggOceanMaps"] = "https://mikkovihtakari.github.io/drat"
  options(repos = repos)
  invisible(repos)
  
  # Install the data package
  
  if (!requireNamespace("ggOceanMapsData", quietly = TRUE)) {
    # message("ggOceanMaps requires ggOceanMapsData, which is not installed. Installing the package.")
    utils::install.packages("ggOceanMapsData")
  }
}

.onAttach <- function(libname, pkgname) {
  if("PlotSvalbard" %in% (.packages())) {
    packageStartupMessage("The ggOceanMaps and PlotSvalbard packages do not function together. Detaching PlotSvalbard.")
    detach("package:PlotSvalbard", unload=TRUE)
  }
}

# Define global variables
utils::globalVariables(c("rdiff.lon", "rdiff.lat"))

# "long", "lat", "group", "round.lat", "round.lon", "n.lat.grid", "n.lon.grid", "lat.interval", "lon.interval", "keep.glaciers", "MapType", "proj4.utm", "n.points", "..level..", "abb", "x", "y", "sal", "temp", "xmin", "xmax", "ymin", "ymax", ".", "variable", "ymin", "ymax", "X", "arctic_bathy", "barents_bathy", "barents_currents", "kongsfjord_watermasses", "svalbard_bathy"
