#' @title Major fisheries areas (hovedomraade) of Norway
#' @docType data
#' @keywords datasets shapefiles fishery
#' @family datasets
#' @name fdir_main_areas
#' @format \code{\link[sf:st_sf]{sf object}} in decimal degrees (EPSG:4326) containing major fishing zones defined by the Norwegian Directorate of Fisheries. Contains also Northwest Atlantic Fisheries Organization's divisions where Norwegian vessels tend to fish.
#' @source \href{https://open-data-fiskeridirektoratet-fiskeridir.hub.arcgis.com/}{Norwegian Directorate of Fisheries} and \href{https://www.nafo.int/About-us/Maps}{Northwest Atlantic Fisheries Organization}
#' @import sf
#' @examples
#' \donttest{ 
#' basemap(fdir_main_areas) + 
#' annotation_spatial(fdir_main_areas, fill = NA)
#' }
"fdir_main_areas"

#' @title Norwegian sub-areas (lokasjon) for commercial fishing
#' @docType data
#' @keywords datasets shapefiles fishery
#' @family datasets
#' @name fdir_sub_areas
#' @format \code{\link[sf:st_sf]{sf object}} in decimal degrees (EPSG:4326) containing major fishing zones defined by the Norwegian Directorate of Fisheries.
#' @source \href{https://open-data-fiskeridirektoratet-fiskeridir.hub.arcgis.com/}{Norwegian Directorate of Fisheries}
#' @import sf
#' @examples 
#' \donttest{
#' basemap(fdir_sub_areas) + 
#' annotation_spatial(fdir_sub_areas, fill = NA)
#' }
"fdir_sub_areas"

#' @title ICES Advisory Areas
#' @docType data
#' @keywords datasets shapefiles fishery
#' @family datasets
#' @name ices_areas
#' @format \code{\link[sf:st_sf]{sf object}} in decimal degrees (EPSG:4326) containing ICES Advisory Areas.
#' @source \href{https://www.ices.dk/}{International Council for the Exploration of the Sea}
#' @import sf
#' @examples
#' \donttest{
#' basemap(ices_areas) + 
#' annotation_spatial(ices_areas, fill = NA)
#' }
"ices_areas"
