#' @title Test whether a limit argument is specified as decimal degrees.
#' @description An internal function to make \code{\link{basemap}} code more readable
#' @param limits A numeric vector of length 1 or 4. See \code{\link{basemap}}
#' @details This is an internal function, which is automatically run by the \code{\link{basemap}} function.
#' @return A logical value
#' @keywords internal
#' @export
#' @author Mikko Vihtakari
#' @seealso \code{\link{basemap}}

is_decimal_limit <- function(limits) {
  if(length(limits) == 1) {
    round(abs(limits), 5) <= 90
  } else if(length(limits) == 4) {
    all(c(round(abs(limits[1:2]), 5) <= 180, round(abs(limits[3:4]), 5) <= 90))
  } else {
    stop("Limits have to be given as a numeric vector of length 1 or 4")
  }
}
