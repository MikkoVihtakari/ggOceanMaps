#' @title Decimal degree bathymetry
#' @docType data
#' @keywords maps shapefiles
#' @family mapfiles
#' @name dd_rbathy
#' @format \link[stars:read_stars]{Raster} bathymetry in decimal degrees (EPSG:4326). Downsampled from ETOPO 60 arc-second grid.
#' @source NOAA National Centers for Environmental Information. 2022: ETOPO 2022 15 Arc-Second Global Relief Model. NOAA National Centers for Environmental Information. \doi{10.25921/fd45-gt74}
#' @importFrom stars read_stars
"dd_rbathy"

#' @title Decimal degree land shapes
#' @docType data
#' @keywords maps shapefiles
#' @family mapfiles
#' @name dd_land
#' @format \link[sf:st_sf]{Simple feature collection} land shapes in decimal degrees (EPSG:4326). Obtained from Natural Earth Data (10m vectors). Includes the islands dataset.
#' @source \href{https://www.naturalearthdata.com/}{Natural Earth Data}
#' @importFrom sf st_sf
"dd_land"

# @title Decimal degree glacier shapes
# @docType data
# @keywords maps shapefiles
# @family mapfiles
# @name dd_glacier
# @format \link[sf:st_sf]{Simple feature collection} glacier shapes in decimal degrees (EPSG:4326).  Obtained from Natural Earth Data (10m vectors). Includes the ice-sheets dataset.
# @source \href{https://www.naturalearthdata.com/}{Natural Earth Data}
# @importFrom sf st_sf
# "dd_glacier"
