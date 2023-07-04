.onLoad <- function(libname, pkgname) {
  options("rgdal_show_exportToProj4_warnings"="none")
  
  # Environment for data download
  
  if(!exists(".ggOceanMapsenv")) .ggOceanMapsenv <- new.env()
  
  # Specify the download folder
  
  if(is.null(.ggOceanMapsenv$datapath)) {
    options(ggOceanMaps.datapath = tempdir())
  } else {
    options(ggOceanMaps.datapath = .ggOceanMapsenv$datapath)
  }
}

.onAttach <- function(libname, pkgname) {
  
  # Check that PlotSvalbard is not attached
  if("package:PlotSvalbard" %in% search()) {
    packageStartupMessage("The ggOceanMaps and PlotSvalbard packages do not function together. Detaching PlotSvalbard.")
    detach("package:PlotSvalbard", unload=TRUE)
  }
  
  if(!exists(".ggOceanMapsenv")) .ggOceanMapsenv <- new.env()
  
  if(is.null(.ggOceanMapsenv$datapath)) {
    msg <- paste0("ggOceanMaps: Setting data download folder to a temporary folder ", 
                  getOption("ggOceanMaps.datapath"),
                  ". This means that any downloaded map data need to be downloaded again when you restart R",
                  ". To avoid this problem, change the default path to a permanent folder on your computer", 
                  ". Add following lines to your .Rprofile file: {",
                  ".ggOceanMapsenv <- new.env(); ",
                  ".ggOceanMapsenv$datapath <- 'YourCustomPath'}",
                  ". You can use usethis::edit_r_profile() to edit the file.", 
                  " normalizePath('~/Documents/ggOceanMapsLargeData') ",
                  "would make it in a writable folder on most operating systems.")
  } else {
    msg <- paste0("ggOceanMaps: Using ", getOption("ggOceanMaps.datapath"),
                  " as data download folder. ",
                  "This folder is customly defined and does not require downloading the detailed map data again.") 
  }
  
  
  packageStartupMessage(paste(strwrap(msg), collapse= "\n"))
  
  # Check and store external dependency versions
  
  #.ggOceanMapsenv$versions <- sf::sf_extSoftVersion()
  #.ggOceanMapsenv$SRS_string <- sf::sf_extSoftVersion()[["GDAL"]] >= "3.0.0" & sf::sf_extSoftVersion()[["PROJ"]] >= "6.0.0"
  
}

# Define global variables
utils::globalVariables(c("rdiff.lon", "rdiff.lat", "label"))


