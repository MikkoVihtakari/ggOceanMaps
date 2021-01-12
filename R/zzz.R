.onLoad <- function(libname, pkgname) {
  options("rgdal_show_exportToProj4_warnings"="none")
}

.onAttach <- function(libname, pkgname) {
  
  # Check that PlotSvalbard is not attached
  if("package:PlotSvalbard" %in% search()) {
    packageStartupMessage("The ggOceanMaps and PlotSvalbard packages do not function together. Detaching PlotSvalbard.")
    detach("package:PlotSvalbard", unload=TRUE)
  }
  
  # Environment for data download
  
  if(!exists(".ggOceanMapsenv")) .ggOceanMapsenv <- new.env()
  
  # Specify the download folder
  
  if(is.null(.ggOceanMapsenv$datapath)) {
    options(ggOceanMaps.datapath = tempdir())
    
    msg <- paste0("Setting data download folder to a temporary folder ", 
                  getOption("ggOceanMaps.datapath"),
                 ". This means that any downloaded map data need to be downloaded again when you restart R",
                 ". To avoid this problem, change the default path to a permanent folder on your computer", 
                 ". Add following lines to your .Rprofile file: {",
                 ".ggOceanMapsenv <- new.env(); ",
                 ".ggOceanMapsenv$datapath <- 'YourCustomPath'}",
                 ". You can use usethis::edit_r_profile() to edit the file.", 
                 " paste(R.home(), 'library', 'ggOceanMapsLargeData', sep = '/') ",
                 "would make the folder in your R package location. Might require administrative rights.")
  
    packageStartupMessage(paste(strwrap(msg), collapse= "\n"))
    
  } else {
    options(ggOceanMaps.datapath = .ggOceanMapsenv$datapath)
    
    msg <- paste0("Using ", getOption("ggOceanMaps.datapath"),
                  " as data download folder. ",
                  "This folder is customly defined and does not require downloading the detailed map data again.")
    
    packageStartupMessage(paste(strwrap(msg), collapse= "\n"))
    
  }
}

# Define global variables
utils::globalVariables(c("rdiff.lon", "rdiff.lat"))

# "long", "lat", "group", "round.lat", "round.lon", "n.lat.grid", "n.lon.grid", "lat.interval", "lon.interval", "keep.glaciers", "MapType", "proj4.utm", "n.points", "..level..", "abb", "x", "y", "sal", "temp", "xmin", "xmax", "ymin", "ymax", ".", "variable", "ymin", "ymax", "X", "arctic_bathy", "barents_bathy", "barents_currents", "kongsfjord_watermasses", "svalbard_bathy"


