#' @title Load large shapefile objects
#' @description Internal function to load large shapefile objects. Downloads the files if they are not found \code{getOption("ggOceanMaps.datapath")}
#' @param x An object from \code{\link{shapefile_list}}.
#' @param force Logical indicating whether to download the file even though it exists. Useful when files in the \href{https://github.com/MikkoVihtakari/ggOceanMapsLargeData}{Github repository have been changed}. Overwrites the old file. 
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. Common users do not need to worry about these details.
#' @return A list of \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrames}}
#' @keywords internal
#' @export
#' @importFrom utils menu download.file
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}

load_map_data <- function(x, force = FALSE) {
  
  # Create the data download folder if it does not exist
  
  if(!dir.exists(getOption("ggOceanMaps.datapath"))) {
    
    msg <- paste0("ggOceanMaps.datapath is pointing to ", getOption("ggOceanMaps.datapath"), ", which does not exist.", " Do you want to create the directory?")
    
    message(paste(strwrap(msg), collapse= "\n"))
    ret.val <- utils::menu(c("Yes", "No"), "")
    
    if(ret.val != 1) {
      msg <- paste0("The ggOceanMaps.datapath ", getOption("ggOceanMaps.datapath"), " does not exist. Cannot download data.")
      stop(paste(strwrap(msg), collapse= "\n"))
    } else {
      dir.create(getOption("ggOceanMaps.datapath"))
      msg <- paste0("The ggOceanMaps.datapath directory created to ", getOption("ggOceanMaps.datapath"))
      message(paste(strwrap(msg), collapse= "\n"))
    }
  }
  
  # Check whether the data has already been downloaded
  
  filepath <- paste(getOption("ggOceanMaps.datapath"), paste0(tolower(x$name), ".rda"), sep = "/")
  
  if(file.exists(filepath) & !force) {
    return(mget(load(filepath)))
  } 
  
  # Download the file, if it does not exist
  
  if(!file.exists(filepath) | force) {
    
    msg <- paste0("You are trying to create a ", x$name, " map.", " Cannot find the required shapefiles from ", getOption("ggOceanMaps.datapath"), ". Do you want download them now?")
    
    message(paste(strwrap(msg), collapse= "\n"))
    ret.val <- utils::menu(c("Yes", "No"), "")
    
    if(ret.val != 1) {
      msg <- paste0(x$name, " shapefiles required to plot the map. Download the file or change the shapefiles argument.")
      stop(paste(strwrap(msg), collapse= "\n"))
    } else {
    download.file(x$path, filepath)
    return(mget(load(filepath)))
    }
  }
}