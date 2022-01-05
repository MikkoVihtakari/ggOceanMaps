#' @title Create a ggplot2 basemap for plotting variables
#' @description Creates a ggplot2 basemap for further plotting of variables.
#' @param x The limit type (\code{limits} or \code{data}) is automatically recognized from the class of this argument.
#' @param limits Map limits. One of the following:
#' \itemize{
#'   \item \strong{numeric vector} of length 4: The first element defines the start longitude, the second element the end longitude (counter-clockwise), the third element the minimum latitude and the fourth element the maximum latitude of the bounding box. The coordinates can be given as decimal degrees or coordinate units for shapefiles used by a projected map. Produces a rectangular map. Latitude limits not given in min-max order are automatically ordered to respect this requirement.
#'   \item \strong{single integer} between 30 and 88 or -88 and -30 produces a polar map for the Arctic or Antarctic, respectively.
#' }
#' Can be omitted if \code{data} or \code{shapefiles} are defined.
#' @param data A data frame, \link[sp]{SpatialPolygons}, or \link[sf]{sf} shape containing longitude and latitude coordinates. If a data frame, the coordinates have to be given in decimal degrees. The limits are extracted from these coordinates and produces a rectangular map. Suited for situations where a certain dataset is plotted on a map. The function attempts to \link[=guess_coordinate_columns]{guess the correct columns} and it is advised to use intuitive column names for longitude (such as "lon", "long", or "longitude") and latitude ("lat", "latitude") columns. Can be omitted if \code{limits} or \code{shapefiles} are defined.
#' @param shapefiles Either a \link[=shapefile_list]{list containing shapefile information} or a character argument referring to a name of pre-made shapefiles in \code{\link{shapefile_list}}. This name is partially matched. Can be omitted if \code{limits} or \code{data} are defined as decimal degrees.
#' @param bathymetry Logical indicating whether bathymetry should be added to the map.
#' @param glaciers Logical indicating whether glaciers and ice-sheets should be added to the map.
#' @param rotate Logical indicating whether the projected maps should be rotated to point towards the pole relative to mid-longitude limit. Experimental.
#' @param bathy.style Character defining the style for bathymetry contours. Alternatives:
#' \itemize{
#' \item \code{"poly_blues"} plots polygons filled with different shades of blue.
#' \item \code{"poly_greys"} plots polygons filled with different shades of gray.
#' \item \code{"contour_blues"} contour lines with different shades of blue.
#' \item \code{"contour_grey"} plots gray contour lines.
#' }
#' @param legends Logical indicating whether the legend for bathymetry should be shown.
#' @param legend.position The position for ggplot2 legend. See the argument with the same name in \link[ggplot2]{theme}.
#' @param lon.interval,lat.interval Numeric value specifying the interval of longitude and latitude grids. \code{NULL} finds reasonable defaults depending on \code{limits}.
#' @param land.col,gla.col,grid.col Character code specifying the color of land, glaciers and grid lines, respectively. Use \code{NA} to remove the grid lines.
#' @param land.border.col,gla.border.col,bathy.border.col Character code specifying the color of the border line for land, glacier, and bathymetry shapes.
#' @param land.size,gla.size,bathy.size,grid.size Numeric value specifying the width of the border line land, glacier and bathymetry shapes as well as the grid lines, respectively. Use the \code{\link{LS}} function for a specific width in pt. See Details.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.
#' @param projection.grid Logical indicating whether the coordinate grid should show projected coordinates instead of decimal degree values. Useful to define limits for large maps in polar regions.
#' @param expand.factor Expansion factor for map limits with the \code{data} argument. Can be used to zoom in and out automatically limited maps. Defaults to 1.1. Set to \code{NULL} to ignore.
#' @param verbose Logical indicating whether information about the projection and guessed column names should be returned as message. Set to \code{FALSE} to make the function silent.
#' @return Returns a \link[ggplot2]{ggplot} map, which can be assigned to an object and modified as any ggplot object.
#' @details The function uses \link[ggplot2:ggplot2-package]{ggplot2}, \link[ggspatial:ggspatial-package]{ggspatial}, GIS packages of R, and shapefiles to plot maps of the world's oceans. 
#' 
#' \strong{Projections}
#' 
#' If the \code{shapefiles} are not specified, the function uses either the \code{limits} or \code{data} arguments to decide which projection to use. Up-to-date conditions are defined in \code{\link{define_shapefiles}} and \code{\link{shapefile_list}} functions. At the time of writing, the function uses three different projections (given as \href{https://epsg.io/}{EPSG codes})
#' \itemize{
#' \item \strong{3995} WGS 84 / Arctic Polar Stereographic. Called "ArcticStereographic". For max latitude (\code{limits[4]}) >= 60 (if min latitude (\code{limits[3]}) >= 30), and single integer latitudes >= 30 and <= 89.
#' \item \strong{3031} WGS 84 / Antarctic Polar Stereographic. Called "AntarcticStereographic". For max latitude (\code{limits[4]}) <= -60 (if min latitude (\code{limits[3]}) <= -30), and single integer latitudes <= -30 and >= -89.
#' \item \strong{4326} WGS 84 / World Geodetic System 1984, used in GPS. Called "DecimalDegree". For min latitude (\code{limits[3]}) < 30 or > -30, max latitude (\code{limits[4]}) < 60 or > -60, and single integer latitudes < 30 and > -30.
#' }
#'
#' \strong{Limits}
#'
#' If the limits are in decimal degrees, the longitude limits (\code{[1:2]}) specify the start and end segments of corresponding angular lines that should reside inside the map area. The longitude limits are defined \strong{counter-clockwise}. The latitude limits \code{[3:4]} define the parallels that should reside inside the limited region given the longitude segments. Note that the actual limited region becomes wider than the polygon defined by the coordinates (shown in Examples). Using \code{data} to limit the map expands the map all around the data points to make them fit into the map. If the limits are given as projected coordinates or as decimal degrees for maps with -60 < latitude < 60, limits elements represent lines encompassing the map area in cartesian space. 
#'
#' \strong{Pre-made shapefiles}
#' 
#' If the limits are not defined as decimal degrees (any longitude outside range [-180, 180] or latitude [-90, 90]), the function will ask to specify \code{shapefiles}. The \code{shapefiles} can be defined by partially matching the names of the pre-made shapefiles in \code{\link{shapefile_list}} (e.g. "Ar" would be enough for "ArcticStereographic") or by specifying custom shapefiles.
#' 
#' \strong{Custom shapefiles}
#' 
#' Custom shapefiles have to be a named list containing at least following elements:
#' \itemize{
#' \item \strong{land} Object name of the \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} containing land. Required.
#' \item \strong{glacier} Object name of the \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} containing glaciers. Use \code{NULL} if glaciers are not needed.
#' \item \strong{bathy} Object name of the \code{\link[sp:SpatialPolygons]{SpatialPolygonsDataFrame}} containing bathymetry contours. Use \code{NULL} if bathymetry is not needed.
#' }
#'
#' See Examples.
#'
#' \strong{Line width and font size}
#'  
#' The line size aesthetics in \link[ggplot2:ggplot2-package]{ggplot2} generates approximately 2.13 wider lines measured in pt than the given values. If you want a specific line width in pt, use the internal function \code{\link{LS}} to convert the desired line width to ggplot2 equivalent. A similar function is also available for font sizes (\code{\link{FS}}).
#' 
#' \strong{CRS warnings}
#' 
#' Open-source GIS systems are rolling over to a new \href{https://rgdal.r-forge.r-project.org/articles/CRS_projections_transformations.html}{to a new projection definition system}. The changes to underlying systems appear to sometimes trigger warnings the user can ignore as long as the resulting map looks OK. Bug reports regarding these warnings are appreciated. 
#'
#' @references Note that if you use this function to generate maps for a publication, it is advised to cite the underlying data. The spatial data used by this function have been acquired from following sources:
#' \itemize{
#' \item \strong{Land polygons.} \href{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}{Natural Earth Data} 1:10m Physical Vectors with the Land and Minor Island datasets combined. Distributed under the \href{https://creativecommons.org/publicdomain/}{CC Public Domain license} (\href{http://www.naturalearthdata.com/about/terms-of-use/}{terms of use}).
#' \item \strong{Glacier polygons.} \href{http://www.naturalearthdata.com/downloads/10m-physical-vectors/}{Natural Earth Data} 1:10m Physical Vectors with the Glaciated Areas and Antarctic Ice Shelves datasets combined. Distributed under the \href{https://creativecommons.org/publicdomain/}{CC Public Domain license} (\href{http://www.naturalearthdata.com/about/terms-of-use/}{terms of use})
#' \item \strong{Bathymetry.} \href{https://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/docs/ETOPO1.pdf}{Amante, C. and B.W. Eakins, 2009. ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24. National Geophysical Data Center, NOAA}. Distributed under the \href{https://www.usa.gov/government-works}{U.S. Government Work license}.
#' }
#' 
#' @family basemap functions
#' @seealso \code{\link[ggplot2]{ggplot}}
#' @author Mikko Vihtakari
#' 
#' @examples
#' # The easiest way to produce a map is to use the limits
#' # argument and decimal degrees:
#' 
#' if(requireNamespace("ggOceanMapsData")) {
#' basemap(limits = 60)
#' }
#' 
#' # Bathymetry and glaciers can be added using the respective arguments:
#' \donttest{
#' basemap(limits = -60, bathymetry = TRUE, glaciers = TRUE)
#' }
#' # The easiest way to add data on the maps is to use the ggspatial functions:
#'
#' dt <- data.frame(lon = c(-150, 150), lat = c(60, 90))
#' \donttest{
#' basemap(data = dt, bathymetry = TRUE) +
#' geom_spatial_point(data = dt, aes(x = lon, y = lat), color = "red")
#' }
#' \dontrun{
#' # Note that writing out data = dt is required because there are multiple
#' # underlying ggplot layers plotted already:
#' basemap(data = dt) +
#' geom_spatial_point(dt, aes(x = lon, y = lat), color = "red")
#' #> Error: `mapping` must be created by `aes()`
#' }
#'
#' # If you want to use native ggplot commands, you need to transform your data
#' # to the projection used by the map:
#' 
#' if(requireNamespace("ggOceanMapsData")) {
#' dt <- transform_coord(dt, bind = TRUE)
#'
#' basemap(data = dt) + geom_point(data = dt, aes(x = lon.proj, y = lat.proj))
#' }
#' \donttest{
#' # The limits argument of length 4 plots a map anywhere in the world:
#' 
#' basemap(limits = c(100, 160, -20, 30), bathymetry = TRUE)
#'
#' # The argument leads to expanded maps towards poles:
#' 
#' dt <- data.frame(lon = c(-160, 160, 160, -160), lat = c(80, 80, 60, 60))
#'
#' basemap(limits = c(160, -160, 60, 80)) +
#' geom_spatial_polygon(data = dt, aes(x = lon, y = lat),
#' fill = NA, color = "red")
#' 
#' # The limits are further expanded when using the data argument:
#'
#' basemap(data = dt) +
#' geom_spatial_polygon(data = dt, aes(x = lon, y = lat),
#' fill = NA, color = "red")
#' 
#' # Rotate:
#' 
#' basemap(data = dt, rotate = TRUE) +
#' geom_spatial_polygon(data = dt, aes(x = lon, y = lat),
#'                     fill = NA, color = "red")
#' 
#' ## To find UTM coordinates to limit a polar map:
#' basemap(limits = 60, projection.grid = TRUE)
#' basemap(limits = c(2.5e4, -2.5e6, 2e6, -2.5e5), shapefiles = "Arctic")
#' 
#' # Using custom shapefiles
#' data(bs_shapes, package = "ggOceanMapsData")
#' basemap(shapefiles = list(land = bs_land, glacier = NULL, bathy = bs_bathy),
#' bathymetry = TRUE)
#' 
#' # grid.col = NA removes grid lines, rotate = TRUE rotates northwards
#' 
#' basemap(limits = c(-180, -140, 50, 70), grid.col = NA, rotate = TRUE)
#'
#' # Rename axis labels
#' 
#' basemap(limits = c(-140, -105, 20, 40), bathymetry = TRUE) + xlab("Lat")
#' 
#' # Remove axis labels
#' 
#' basemap(limits = c(0, 60, 68, 80)) + labs(x = NULL, y = NULL)
#'
#' basemap(limits = c(0, 60, 68, 80), rotate = TRUE) +
#' theme(axis.title = element_blank(),
#'       axis.text = element_blank(),
#'       axis.ticks.x = element_blank(),
#'       axis.ticks.y = element_blank()
#'       )
#' }
#' @import ggplot2 ggspatial sp sf
#' @export

## Test parameters
# limits = NULL; data = NULL; shapefiles = NULL; bathymetry = FALSE; glaciers = FALSE; rotate = FALSE; legends = TRUE; legend.position = "right"; lon.interval = NULL; lat.interval = NULL; bathy.style = "poly_blues"; bathy.border.col = NA; bathy.size = 0.1; land.col = "grey60"; land.border.col = "black"; land.size = 0.1; gla.col = "grey95"; gla.border.col = "black"; gla.size = 0.1; grid.col = "grey70"; grid.size = 0.1; base_size = 11; projection.grid = FALSE; verbose = TRUE

basemap <- function(x = NULL, limits = NULL, data = NULL, shapefiles = NULL, bathymetry = FALSE, glaciers = FALSE, rotate = FALSE, legends = TRUE, legend.position = "right", lon.interval = NULL, lat.interval = NULL, bathy.style = "poly_blues", bathy.border.col = NA, bathy.size = 0.1, land.col = "grey60", land.border.col = "black", land.size = 0.1, gla.col = "grey95", gla.border.col = "black", gla.size = 0.1, grid.col = "grey70", grid.size = 0.1, base_size = 11, projection.grid = FALSE, expand.factor = 1.1, verbose = FALSE) {
  
  # Install ggOceanMapsData if not installed
  if (!requireNamespace("ggOceanMapsData", quietly = TRUE)) {
    stop('The ggOceanMapsData package needs to be installed for ggOceanMaps to function.\nInstall the data package by running\ninstall.packages("ggOceanMapsData", repos = c("https://mikkovihtakari.github.io/drat", "https://cloud.r-project.org")\nOR\ndevtools::install_github("MikkoVihtakari/ggOceanMapsData")')
  }
  
  # The x argument to limits or data
  
  if(!is.null(x)) {
    if(any(class(x) %in% c("integer", "numeric")) & is.null(limits)) {
      limits <- x
    } else if(any(class(x) %in% c("data.table", "sf", "SpatialPolygonsDataFrame", "SpatialPolygons")) & is.null(limits) & is.null(data)) {
      data <- x
    }
  }
  
  # Checks ####
  
  if(is.null(data) & is.null(limits) & is.null(shapefiles)) stop("One or several of the arguments limits, data and shapefiles is required.")
  if(class(legends) != "logical" | !length(legends) %in% 1:2) stop("'legends' argument has to be a logical vector of length 1 or 2. Read the explantion for the argument in ?basemap")
  
  ###########
  # Data ####
  
  X <- basemap_data(limits = limits, data = data, shapefiles = shapefiles, bathymetry = bathymetry, glaciers = glaciers, lon.interval = lon.interval, lat.interval = lat.interval, rotate = rotate, expand.factor = expand.factor, verbose = verbose)
  
  ###########
  # Plot ####
  
  ## Bathymetry data
  
  if(bathymetry & !is.null(X$shapefiles$bathy)) {
    
    bathy_cmd <- switch(bathy.style,
                        poly_blues = "bathy_pb",
                        poly_greys = "bathy_pg",
                        contour_blues = "bathy_cb",
                        contour_grey = "bathy_cg",
                        stop(paste("bathy.style not found"))
    )
    
    bathy.legend <- ifelse(length(legends) == 1, legends, legends[1])
    
    if(bathy_cmd == "bathy_cg" & is.na(bathy.border.col)) bathy.border.col <- "grey"
    
    layers <- paste(map_cmd("base"), map_cmd(bathy_cmd), sep = " + ")
    
  } else {
    layers <- map_cmd("base")
  }
  
  ## Land
  
  if (length(X$shapefiles$land) > 0) {
    layers <- paste(layers, map_cmd("land"), sep = " + ")
  }
  
  ## Glaciers
  
  if(glaciers & !is.null(X$shapefiles$glacier)) {
    if(length(X$shapefiles$glacier) > 0) {
      layers <- paste(layers, map_cmd("glacier"), sep = " + ")
    }
  }
  
  ## Grid and definitions
  
  if(X$polar.map) {
    if(projection.grid) {
      layers <- paste(layers, map_cmd("defs_polar_proj"), sep = " + ")
    } else {
      layers <- paste(layers, map_cmd("defs_polar"), sep = " + ")
    }
  } else {
    if(projection.grid) {
      layers <- paste(layers, map_cmd("defs_rect_proj"), sep = " + ")
    } else {
      layers <- paste(layers, map_cmd("defs_rect"), sep = " + ")
    }
  }
  
  ## Final plotting
  
  out <- eval(parse(text=layers))
  
  attributes(out)$class <- c(attributes(out)$class, "ggOceanMaps")
  
  attributes(out)$bathymetry <- bathymetry
  attributes(out)$glaciers <- glaciers
  attributes(out)$limits <- X$map.limits
  attributes(out)$polarmap <- X$polar.map
  attributes(out)$crs <- X$shapefiles$crs
  attributes(out)$proj <- X$proj
  
  out
  
  ## END ####
}