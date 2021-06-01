#' @title Guess longitude and latitude columns in a data frame
#' @description An internal function to make \code{\link{basemap}} code more readable
#' @param data Dataframe containing data for which the limits should be calculated.
#' @param lon,lat Character defining the name of the longitude and latitude columns in \code{data}. Use \code{NULL} to guess the longitude and/or latitude columns in \code{x}.
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function.
#' @return A named vector of colummn names. 
#' @keywords internal
#' @export
#' @author Mikko Vihtakari
#' @seealso \code{\link{auto_limits}}, \code{\link{transform_coord}}, \code{\link{basemap}}

guess_coordinate_columns <- function(data, lon = NULL, lat = NULL) {

    if(!"data.frame" %in% class(data)) stop("data argument has to be a data.frame")

    if(is.null(lon)) {
      lon <- colnames(data)[grep("^lon$|longitude|^long$", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(data)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
      if(is.na(lon)) {
        lon <- colnames(data)[grep("lon", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(data)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
      }
      if(is.na(lon)) {
        lon <- colnames(data)[grep("^x$", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(data)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
      }
    }

    if(is.null(lat)) {
      lat <- colnames(data)[grep("^lat$|latitude", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(data)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
      if(is.na(lat)) {
        lat <- colnames(data)[grep("lat", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(data)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
      }
      if(is.na(lat)) {
        lat <- colnames(data)[grep("^y$", gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", gsub("[[:punct:]]", " ", colnames(data)), perl = TRUE), ignore.case = TRUE, perl = TRUE)][1]
      }
    }

  c(lon = lon, lat = lat)
}
