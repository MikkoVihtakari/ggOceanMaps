#' @title Load large shapefile objects
#' @description Internal function to load large shapefile objects. Downloads the files if they are not found \code{getOption("ggOceanMaps.datapath")}
#' @param x An object from \code{\link{shapefile_list}}.
#' @param force Logical indicating whether to download the file even though it exists. Useful when files in the \href{https://github.com/MikkoVihtakari/ggOceanMapsLargeData}{Github repository have been changed}. Overwrites the old file. 
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function. Common users do not need to worry about these details.
#' @return A list of spatial objects
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
  
  # Create file paths
  
  x[c("land", "glacier", "bathy")] <- 
    lapply(x[c("land", "glacier", "bathy")], function(k) {
    
    if(is.null(k)) {
      NULL
    } else if(grepl("/", k)) {
      if(!grepl(".rda", k)) {
        paste0(k, ".rda")
      } else {
        k
      }
    } else {
      k
      #paste(getOption("ggOceanMaps.datapath"), paste0(tolower(x$name), ".rda"), sep = "/")
    }
  })
  
  # Check whether the data has already been downloaded
  
  exist_list <- lapply(x[c("land", "glacier", "bathy")], function(k) {
    if(is.null(k)) {
      NULL
    } else if(grepl("/", k)) {
      if(file.exists(k) & !force) {
        TRUE
      } else {
        FALSE
      }
    } else {
      TRUE
    }
  })
  
  exist_list[sapply(exist_list, is.null)] <- NULL
  
  # Download missing files, if they do not exist
  
  if(any(!unlist(exist_list))) {
    missing_files <- paste(sapply(names(exist_list)[!unlist(exist_list)], function(k) x[[k]]), collapse = ", ")
    msg <- paste0("Cannot find files: ", missing_files, ". Do you want download them now?") 
    message(paste(strwrap(msg), collapse= "\n"))
    ret.val <- utils::menu(c("Yes", "No"), "")
    
    if(ret.val != 1) {
      msg <- paste0(missing_files, " shapefiles required to plot the map. Download the file(s) or change the shapefiles argument.")
      stop(paste(strwrap(msg), collapse= "\n"))
    }
  }
  
  x[c("land", "glacier", "bathy")] <-
    lapply(x[c("land", "glacier", "bathy")], function(k) {
    if(is.null(k)) {
      NULL
    } else if(grepl("/", k)) {
      if(file.exists(k) & !force) {
        mget(load(k))[[1]]
      } else {
        download.file(paste(x$path, k), getOption("ggOceanMaps.datapath"))
        mget(load(getOption("ggOceanMaps.datapath")))
      }
    } else {
      eval(parse(text = k))
    }
  })
  
  # Return
  x[sapply(x, is.null)] <- NULL
  
  return(x)
}