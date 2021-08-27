##' @description Temporary workaround for the specification of projargs in CRS(), required by migration to
##' GDAL >= 3 and PROJ >= 6.
##' @title CRSargs, system specific epsg code string formating for CRS.
##' @keywords internal
##' @param epsg EPSG code of the projection (coercible to a character string).
##' @return A character string that can be used as projargs in the CRS (sp) function: either a SRS_string
##' (e.g. \code{"EPSG:4326"}) or a "+init" PROJ string (e.g. \code{"+init=epsg:4326"}), depending on the libraries and
##' packages versions used.
##' @author Yves Reecht
##' @importFrom sf sf_extSoftVersion
##' @importFrom utils compareVersion
##' @details Test the version of GDAL, PROJ and sp, and return the appropriate format.
##' @examples
##' CRSargs()
##' CRS(projargs = CRSargs())
##' CRS(projargs = CRSargs(epsg = 27700))
##' @export
CRSargs <- function(epsg = 4326)
{
    ## Minimal versions for CRS() to support SRS_string:
    minVersions <- c("GDAL" = "3.0", "PROJ" = "6.0", "sp" = "1.4-4")

    installed <- c(sf::sf_extSoftVersion(),
                   sp = as.character(packageVersion("sp")))[names(minVersions)]

    res <- sapply(names(minVersions),
                  function(i)
           {
               utils::compareVersion(installed[i],
                                     minVersions[i])
           })

    return(paste0(ifelse(any(res < 0), "+init=epsg:", "EPSG:"),
                  epsg))
}



