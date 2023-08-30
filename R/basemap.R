#' @title Create a ggplot2 basemap for plotting variables
#' @description Creates a ggplot2 basemap for further plotting of data.
#' @param x The limit type (\code{limits}, \code{data}, or \code{shapefiles}) is automatically recognized from the class of this argument.
#' @param limits Map limits. One of the following:
#' \itemize{
#'   \item \strong{numeric vector} of length 4: The first element defines the start longitude, the second element the end longitude (counter-clockwise), the third element the minimum latitude, and the fourth element the maximum latitude of the bounding box. Also accepts \code{\link[sf:st_bbox]{sf::st_bbox}} type named vectors with limits in any order. The coordinates can be given as decimal degrees or coordinate units for shapefiles used by a projected map. Produces a rectangular map. Latitude limits not given in min-max order are automatically ordered to respect this requirement.
#'   \item \strong{single integer} between 30 and 88 or -88 and -30 produces a polar map for the Arctic or Antarctic, respectively.
#' }
#' Can be omitted if \code{data} or \code{shapefiles} are defined.
#' @param data A data frame, sp, or \link[sf]{sf} shape containing longitude and latitude coordinates. If a data frame, the coordinates have to be given in decimal degrees. The limits are extracted from these coordinates and produce a rectangular map. Suited for situations where a certain dataset is plotted on a map. The function attempts to \link[=guess_coordinate_columns]{guess the correct columns} and it is advised to use intuitive column names for longitude (such as "lon", "long", or "longitude") and latitude ("lat", "latitude") columns. Can be omitted if \code{limits} or \code{shapefiles} are defined.
#' @param shapefiles Either a \link[=shapefile_list]{list containing shapefile information} or a character argument referring to a name of pre-made shapefiles in \code{\link{shapefile_list}}. This name is partially matched. Can be omitted if \code{limits} or \code{data} is defined as decimal degrees.
#' @param crs \link[sf:st_crs]{Coordinate reference system} (CRS) for the map. If \code{NULL} (default), the CRS is selected automatically based on \code{limits}, \code{data}, or \code{shapefiles}. Passed to \code{\link[sf]{st_crs}}. Typically integers giving the EPGS code are the easiest. Cannot be used simultaneously with \code{rotate}.
#' @param bathymetry Logical indicating whether bathymetry should be added to the map. Functions together with \code{bathy.style}. See Details.
#' @param glaciers Logical indicating whether glaciers and ice sheets should be added to the map.
#' @param rotate Logical indicating whether the projected maps should be rotated to point towards the pole relative to the mid-longitude limit. 
#' @param bathy.style Character (plots bathymetry; list of alternatives in Details) or \code{NULL} ("raster_binned_blues" if \code{bathymetry = TRUE}) defining the bathymetry style. Partially matched, can be abbreviated, and used to control bathymetry plotting together with \code{bathymetry}. See Details. 
#' @param downsample Integer defining the downsampling rate for raster bathymetries. A value of 0 (default) does not downsample, 1 skips every second row, 2 every second and third. See \code{\link[stars]{geom_stars}}
#' @param legends Logical indicating whether the legend for bathymetry should be shown.
#' @param legend.position The position for ggplot2 legend. See the argument with the same name in \link[ggplot2]{theme}.
#' @param lon.interval,lat.interval Numeric value specifying the interval of longitude and latitude grids. \code{NULL} finds reasonable defaults depending on \code{limits}.
#' @param land.col,gla.col,grid.col Character code specifying the color of land, glaciers, and grid lines, respectively. Use \code{NA} to remove the grid lines.
#' @param land.border.col,gla.border.col,bathy.border.col Character code specifying the color of the border line for land, glacier, and bathymetry shapes.
#' @param land.size,gla.size,bathy.size,grid.size Numeric value specifying the width of the border line land, glacier and bathymetry shapes as well as the grid lines, respectively. Use the \code{\link{LS}} function for a specific width in pt. See Details.
#' @param bathy.alpha Transparency parameter for the bathymetry fill color. See \link[ggplot2]{scale_alpha}.
#' @param base_size Base size parameter for ggplot. See \link[ggplot2]{ggtheme}.
#' @param projection.grid Logical indicating whether the coordinate grid should show projected coordinates instead of decimal degree values. Useful to define limits for large maps in polar regions.
#' @param expand.factor Expansion factor for map limits with the \code{data} argument. Can be used to zoom in and out automatically limited maps. Defaults to 1.1. Set to \code{NULL} to ignore.
#' @param verbose Logical indicating whether information about the projection and guessed column names should be returned as messages. Set to \code{FALSE} to make the function silent.
#' @return Returns a \link[ggplot2]{ggplot} map, which can be assigned to an object and modified as any ggplot object.
#' @details The function uses \link[ggplot2:ggplot2-package]{ggplot2}, \link[sf:sf]{sf}, \link[stars:st_as_stars]{stars} and spatial files to plot maps of the world's oceans. 
#' 
#' \strong{Limits}
#'
#' If the limits are in decimal degrees, the longitude limits (\code{[1:2]}) specify the start and end segments of corresponding angular lines that should reside inside the map area. The longitude limits are defined \strong{counter-clockwise}. The latitude limits \code{[3:4]} define the parallels that should reside inside the limited region given the longitude segments. Note that the actual limited region becomes wider than the polygon defined by the coordinates (shown in Examples). Using \code{data} to limit the map expands the map all around the data points to make them fit into the map. If the limits are given as projected coordinates or as decimal degrees for maps with -60 < latitude < 60, limit elements represent lines encompassing the map area in cartesian space. 
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
#' The \code{rotate} argument changes the pre-defined projection such that mid-longitude point in the map points northward. 
#' 
#' The \code{crs} argument can be used to define the projection, which can be useful when plotting, for instance, model data that are difficult to transform into another projection. 
#' 
#' \strong{Bathymetry}
#' 
#' Bathymetry can be plotted by simply specifying \code{bathymetry = TRUE} or \code{bathy.style} (you won't need to specify both any longer). The former uses a low-resolution raster file shipped with ggOceanMaps. The package contains an option to plot higher resolution bathymetries than the default binned blue alternative (\code{bathy.style = "raster_binned_blues"}). These bathymetries can be accessed by specifying the \code{bathy.style} argument and require a download from \href{https://github.com/MikkoVihtakari/ggOceanMapsLargeData}{ggOceanMapsLargeData} or other online repositories. The \code{bathy.style} character argument consists of three parts separated by a \code{_}. The first part gives the type: raster, poly(gon), or contour. The two latter ones use vector data. The second part gives the resolution: binned, continuous or user. The continuous and user options cannot be used for vector data. The user option accepts any raster file that can be opened using \link[stars]{read_stars}. The path to the file has to be stored in \code{ggOceanMaps.userpath} \link[base:options]{option} (e.g. \code{options(ggOceanMaps.userpath = "PATH_TO_THE_FILE")}) (you can set this in .Rprofile to avoid having to type it every time). The last part defines the color: blues or grays. These options can be abbreviated by specifying the first letter of each part. Gray contour lines are an exception to the rule above and can be plotted using \code{bathy.style = "contour_gray"}. Future versions may contain a combination of raster and gray contours, but these have not been implemented yet. Currently implemented \code{bathy.style} alternatives are:
#' \itemize{
#' \item \code{NULL} (\strong{default}). Bathymetry style is searched from \code{getOption("ggOceanMaps.bathy.style")}. If not found, \code{"raster_binned_blues"} is used. 
#' \item \code{"raster_binned_blues"} or \code{"rbb"} plots binned raster bathymetry filled with different shades of blue. Does not require a download.
#' \item \code{"raster_binned_grays"} or \code{"rbg"} the same than above but uses different shades of gray.
#' \item \code{"raster_continuous_blues"} or \code{"rcb"} plots continuous raster bathymetry filled with different shades of blue. More detailed and visually more appealing than the binned bathymetry. Recommended. Requires a download. 
#' \item \code{"raster_continuous_grays"} or \code{"rcg"} the same than above but uses different shades of gray.
#' \item \code{"raster_user_blues"} or \code{"rub"} plots continuous raster bathymetry filled with different shades of blue from \code{getOption("ggOceanMaps.user.path")}. Any file supported by \link[stars]{read_stars} should work. The file has to be placed into the location specified by the path. Experimental feature. Has been tested using \href{https://www.ncei.noaa.gov/products/etopo-global-relief-model}{ETOPO 60 arc-second} and \href{https://www.gebco.net/data_and_products/gridded_bathymetry_data/}{GEBCO 15 arc-second} grids. Please report any bugs you find. 
#' \item \code{"raster_user_grays"} or \code{"rug"} the same than above but uses different shades of gray.
#' \item \code{"poly_binned_blues"}, \code{"poly_blues"}, \code{"pbb"} or \code{"pb"} plots polygon bathymetry filled with different shades of blue. Default in the versions older than 2.0 of ggOceanMaps. Requires a download. 
#' \item \code{"poly_binned_grays"}, \code{"poly_grays"}, \code{"pbg"} or \code{"pg"} same than above but uses different shades of gray.
#' \item \code{"contour_binned_blues"}, \code{"contour_blues"}, \code{"cbb"} or \code{"cb"} contour lines with different shades of blue. Requires a download.
#' \item \code{"contour_gray"}, \code{"contour_gray"} or \code{"cg"} plots gray contour lines. Requires a download. 
#' }
#' 
#' The default can be changed by setting the \code{ggOceanMaps.bathy.style} option. \code{options(ggOceanMaps.bathy.style = "poly_blues")} would make the style similar to older pre-2.0 versions of ggOceanMaps. 
#' 
#' \strong{Pre-made shapefiles}
#' 
#' If the limits are not defined as decimal degrees (any longitude outside the range [-180, 180] or latitude [-90, 90]), the function will ask to specify \code{shapefiles}. The \code{shapefiles} can be defined by partially matching the names of the pre-made shapefiles in \code{\link{shapefile_list}} (e.g. "Ar" would be enough for "ArcticStereographic") or by specifying custom shapefiles.
#' 
#' \strong{Custom shapefiles}
#' 
#' Custom shapefiles have to be a named list containing at least the following elements:
#' \itemize{
#' \item \strong{land} Object name of the \link[sf:st_sf]{spatial polygon} containing land. Required.
#' \item \strong{glacier} Object name of the \link[sf:st_sf]{spatial polygon} containing glaciers. Not required if glaciers are not needed.
#' \item \strong{bathy} Object name of the \link[sf:st_sf]{spatial polygon} or \link[stars:st_as_stars]{raster} containing bathymetry data. Not required if bathymetry is not needed.
#' }
#'
#' See Examples.
#'
#' \strong{Line width and font size}
#'  
#' The line size aesthetics in \link[ggplot2:ggplot2-package]{ggplot2} generates approximately 2.13 wider lines measured in pt than the given values. If you want a specific line width in pt, use the internal function \code{\link{LS}} to convert the desired line width to the ggplot2 equivalent. A similar function is also available for font sizes (\code{\link{FS}}).
#' 
#' @references Note that if you use this function to generate maps for a publication, it is advised to cite the underlying data. The spatial data used by this function have been acquired from the following sources:
#' \itemize{
#' \item \strong{Land polygons.} \href{https://www.naturalearthdata.com/downloads/10m-physical-vectors/}{Natural Earth Data} 1:10m Physical Vectors with the Land and Minor Island datasets combined. Distributed under the \href{https://creativecommons.org/publicdomain/}{CC Public Domain license} (\href{https://www.naturalearthdata.com/about/terms-of-use/}{terms of use}).
#' \item \strong{Glacier polygons.} \href{https://www.naturalearthdata.com/downloads/10m-physical-vectors/}{Natural Earth Data} 1:10m Physical Vectors with the Glaciated Areas and Antarctic Ice Shelves datasets combined. Distributed under the \href{https://creativecommons.org/publicdomain/}{CC Public Domain license} (\href{https://www.naturalearthdata.com/about/terms-of-use/}{terms of use})
#' \item \strong{Bathymetry.} \href{https://www.ncei.noaa.gov/products/etopo-global-relief-model}{NOAA National Centers for Environmental Information. 2022: ETOPO 2022 15 Arc-Second Global Relief Model. NOAA National Centers for Environmental Information.} \doi{10.25921/fd45-gt74}. Distributed under the \href{https://www.usa.gov/government-works/}{U.S. Government Work license}.
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
#' basemap(limits = 60) # synonym to basemap(60)
#' \donttest{
#' # Bathymetry can be added using the respective argument:
#' basemap(limits = -60, bathymetry = TRUE)
#' 
#' \dontrun{
#' # Glaciers require a download in the new version:
#' basemap(limits = -60, glaciers = TRUE, shapefiles = "Arctic")
#' }
#' 
#' # The easiest way to add data on the maps is to use the ggspatial functions:
#' dt <- data.frame(lon = c(-150, 150), lat = c(60, 90))
#' if(requireNamespace("ggspatial", quietly = TRUE)) {
#' basemap(data = dt, bathymetry = TRUE) +
#'   ggspatial::geom_spatial_point(data = dt, aes(x = lon, y = lat), 
#'     color = "red")
#' }
#' \dontrun{
#' # Note that writing out data = dt is required because there are multiple
#' # underlying ggplot layers plotted already:
#' basemap(data = dt) +
#' ggspatial::geom_spatial_point(dt, aes(x = lon, y = lat), color = "red")
#' #> Error: `mapping` must be created by `aes()`
#' }
#'
#' # If you want to use native ggplot commands, you need to transform your data
#' # to the projection used by the map:
#' dt <- transform_coord(dt, bind = TRUE)
#'
#' basemap(data = dt) + 
#'   geom_point(data = dt, aes(x = lon.proj, y = lat.proj), color = "red")
#'
#' # The limits argument of length 4 plots a map anywhere in the world:
#' basemap(limits = c(100, 160, -20, 30), bathymetry = TRUE)
#'
#' # The limits are further expanded when using the data argument:
#' 
#' dt <- data.frame(lon = c(-160, 160, 160, -160), lat = c(80, 80, 60, 60))
#'
#' if(requireNamespace("ggspatial", quietly = TRUE)) {
#' basemap(data = dt) +
#'   ggspatial::geom_spatial_polygon(data = dt, aes(x = lon, y = lat),
#'     fill = NA, color = "red")
#' 
#' # Rotate:
#' basemap(data = dt, rotate = TRUE) +
#'   ggspatial::geom_spatial_polygon(data = dt, aes(x = lon, y = lat),
#'     fill = NA, color = "red")
#' }
#' 
#' # Alternative:
#' basemap(data = dt, rotate = TRUE) +
#'   geom_polygon(data = transform_coord(dt, rotate = TRUE), 
#'     aes(x = lon, y = lat), fill = NA, color = "red")
#' 
#' ## To find UTM coordinates to limit a polar map:
#' basemap(limits = 60, projection.grid = TRUE)
#' 
#' \dontrun{
#' # (Arctic shapes require a download in 2.0)
#' basemap(limits = c(2.5e4, -2.5e6, 2e6, -2.5e5), shapefiles = "Arctic")
#' 
#' # Using custom shapefiles (requires download):
#' data(bs_shapes, package = "ggOceanMapsData")
#' basemap(shapefiles = list(land = bs_land))#' 
#' 
#' # Premade shapefiles from ggOceanMapsLargeData (requires download):
#' basemap("BarentsSea", bathymetry = TRUE)
#' }
#' 
#' # grid.col = NA removes grid lines, rotate = TRUE rotates northwards: 
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
#' @import ggplot2
#' @export

## Test parameters

# limits = c(160, -160, 60, 80); bathymetry = TRUE
# x = NULL; limits = NULL; data = NULL; shapefiles = NULL; crs = NULL; bathymetry = FALSE; glaciers = FALSE; rotate = FALSE; legends = TRUE; legend.position = "right"; lon.interval = NULL; lat.interval = NULL; bathy.style = NULL; downsample = 0; bathy.border.col = NA; bathy.size = 0.1; bathy.alpha = 1; land.col = "grey60"; land.border.col = "black"; land.size = 0.1; gla.col = "grey95"; gla.border.col = "black"; gla.size = 0.1; grid.col = "grey70"; grid.size = 0.1; base_size = 11; projection.grid = FALSE; verbose = TRUE

basemap <- function(x = NULL, limits = NULL, data = NULL, shapefiles = NULL, crs = NULL, bathymetry = FALSE, glaciers = FALSE, rotate = FALSE, legends = TRUE, legend.position = "right", lon.interval = NULL, lat.interval = NULL, bathy.style = NULL, downsample = 0, bathy.border.col = NA, bathy.size = 0.1, bathy.alpha = 1, land.col = "grey60", land.border.col = "black", land.size = 0.1, gla.col = "grey95", gla.border.col = "black", gla.size = 0.1, grid.col = "grey70", grid.size = 0.1, base_size = 11, projection.grid = FALSE, expand.factor = 1.1, verbose = FALSE) {
  
  # The x argument to limits or data
  
  if(!is.null(x)) {
    if(inherits(x, c("integer", "numeric", "bbox")) & is.null(limits)) {
      limits <- x
    } else if(inherits(x, c("data.frame", "data.table", "sf", "sfc", "SpatialPolygonsDataFrame", "SpatialPolygons", "SpatialPoints", "SpatialPointsDataFrame")) & is.null(limits) & is.null(data)) {
      data <- x
    } else if(inherits(x, c("character", "list"))) {
      shapefiles <- x
    }
  }
  
  # Bathymetry style
  
  if(is.null(bathy.style)) {
    bathy.style <- ifelse(!is.null(getOption("ggOceanMaps.bathy.style")), getOption("ggOceanMaps.bathy.style"), "raster_binned_blues")
  } else {
    bathymetry <- TRUE
  }
  
  bathy_cmd <- define_bathy_style(bathy.style)
  bathy.type <- gsub("_blues$|_grays$", "", names(bathy_cmd))
  bathy.type <- ifelse(grepl("raster", bathy.type), bathy.type, "vector")
  bathy_color <- utils::tail(unlist(strsplit(names(bathy_cmd), "_")), n = 1)
  
  if(bathymetry & !is.null(shapefiles) & inherits(shapefiles, "list")) {
    if(!is.null(shapefiles$bathy)) {
      if(grepl("raster", bathy.type) & !inherits(shapefiles$bathy$raster, c("stars", "stars_proxy"))) {
        if(inherits(shapefiles$bathy$raster, c("sf", "sfc", "SpatialPolygonsDataFrame", "SpatialPolygons"))) {
          msg <- paste0("Detecting vector bathymetry. Code written for <2.0 perhaps? Changing bathy.style to 'poly_blues'.")
          
          message(paste(strwrap(msg), collapse= "\n"))
          bathy.style <- "poly_blues"
          
          bathy_cmd <- define_bathy_style(bathy.style)
          bathy.type <- gsub("_blues$|_grays$", "", names(bathy_cmd))
          bathy.type <- ifelse(grepl("raster_binned|raster_continuous", bathy.type), bathy.type, "vector")
        } 
      }
    } else {
      stop("shapefiles = list(..., bathy) is required when using custom shapefiles with bathymetry = TRUE")
    }
  }
  
  
  # Checks ####
  
  if(is.null(data) & is.null(limits) & is.null(shapefiles)) stop("One or several of the arguments limits, data and shapefiles is required.")
  if(!is.logical(legends) | !length(legends) %in% 1:2) stop("'legends' argument has to be a logical vector of length 1 or 2. Read the explantion for the argument in ?basemap")
  if(!is.null(crs) & rotate) {
    rotate <- FALSE
    message("The rotate argument cannot be used with custom crs. Turning rotate to FALSE.")
  }
  
  ###########
  # Data ####
  
  X <- basemap_data(limits = limits, data = data, shapefiles = shapefiles, crs = crs, bathymetry = bathymetry, bathy.type = bathy.type, downsample = downsample, glaciers = glaciers, lon.interval = lon.interval, lat.interval = lat.interval, rotate = rotate, expand.factor = expand.factor, verbose = verbose)
  
  ###########
  # Plot ####
  
  ## Bathymetry data
  
  if(bathymetry & !is.null(X$shapefiles$bathy)) {
    
    bathy.legend <- ifelse(length(legends) == 1, legends, legends[1])
    
    if(bathy_cmd == "bathy_cg" & is.na(bathy.border.col)) bathy.border.col <- "grey"
    
    if(bathy.type == "raster_binned") {
      layers <- paste(
        map_cmd("base"), map_cmd(bathy_cmd), 
        ifelse(bathy_color == "blues", map_cmd("bathy_rbb_scale"), map_cmd("bathy_rbg_scale")), 
        sep = " + ")
    } else if(bathy.type %in% c("raster_user", "raster_continuous")) {
      layers <- paste(
        map_cmd("base"), map_cmd(bathy_cmd), 
        ifelse(bathy_color == "blues", map_cmd("bathy_rcb_scale"), map_cmd("bathy_rcg_scale")), 
        sep = " + ")
    } else {
      layers <- paste(map_cmd("base"), map_cmd(bathy_cmd), sep = " + ")
    }
    
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
  
  ## An expand bug, try with basemap(limits = c(-120, -0, -60, -90), rotate = TRUE)
  
  if(diff(X$map.limits[3:4]) > 1e6 && X$map.limits[3] == 0) X$map.limits[3] <- 1
  
  ## Final plotting
  
  out <- eval(parse(text=layers))
  
  attributes(out)$class <- c(attributes(out)$class, "ggOceanMaps")
  
  attributes(out)$bathymetry <- bathymetry
  attributes(out)$glaciers <- glaciers
  attributes(out)$limits <- X$map.limits
  attributes(out)$polarmap <- X$polar.map
  attributes(out)$map.grid <- X$map.grid
  attributes(out)$crs <- sf::st_crs(X$proj)
  
  suppressWarnings(out)
  
  ## END ####
}