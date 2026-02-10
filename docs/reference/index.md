# Package index

## Basemap functions

Maps are plotted using the [`basemap()`](../reference/basemap.md) or
[`qmap()`](../reference/qmap.md) functions that work almost similarly to
[`ggplot()` as a
base](https://ggplot2.tidyverse.org/reference/index.html) for adding
further layers to the plot using the `+` operator. The maps generated
this way already contain multiple ggplot layers. Consequently, the
[`data` argument](https://ggplot2.tidyverse.org/reference/ggplot.html)
needs to be explicitly specified inside `geom_*` functions when adding
`ggplot2` layers. Depending on the location of the map, the underlying
coordinates may be projected. Decimal degree coordinates need to be
transformed to the projected coordinates using the `transform_coord`,
[ggspatial](https://paleolimbot.github.io/ggspatial/), or [`geom_sf`
functions.](https://ggplot2.tidyverse.org/reference/ggsf.html). The
`shapefile_list` function can be used to acquire `shapefiles` argument
names when limiting basemaps using projected coordinates.

- [`basemap()`](basemap.md) : Create a ggplot2 basemap for plotting
  variables

- [`qmap()`](qmap.md) : Quick map

- [`transform_coord()`](transform_coord.md) : Transform spatial
  coordinates to another projection

- [`shapefile_list()`](shapefile_list.md) :

  A list of pre-made shapefiles for `basemap`

- [`dist2land()`](dist2land.md) : Calculate distance to the closest land
  for coordinates

- [`get_depth()`](get_depth.md) : Extract depth for coordinates from a
  raster bathymetry dataset

## Functions to help customizing basemaps

These functions help in customizing basemaps. The `auto_limits` function
can be used to retrieve projected coordinates from decimal degree
position data and the `reorder_layers` can be used to move land and
glaciers on top of added ggplot2 data layers.

- [`auto_limits()`](auto_limits.md) : Automatic limits for basemap

- [`reorder_layers()`](reorder_layers.md) :

  Move `basemap` land, glacier and grid layers on top of other ggplot
  layers

- [`theme_map()`](theme_map.md) : A ggplot2 theme for maps

## Create bathymetries and shapefiles

Custom bathymetry shapes can be added using the `raster_bathymetry` and
`vector_bathymetry` functions in this order. The underlying data can be
retrieved from the
[ETOPO](https://www.ncei.noaa.gov/products/etopo-global-relief-model) or
[GEBCO](https://www.gebco.net/data_and_products/gridded_bathymetry_data/)
repositories. Already existing
[Geonorge](https://kartkatalog.geonorge.no/metadata/kartverket/dybdedata/2751aacf-5472-4850-a208-3532a51c529a)
bathymetries can be opened and added to the `shapefiles` argument as
`list(bathy =)`. The `clip_shapefile` function can be used to crop
already existing SpatialPolygons.

- [`raster_bathymetry()`](raster_bathymetry.md) : Simplify a bathymetry
  raster ready for vectorization
- [`vector_bathymetry()`](vector_bathymetry.md) : Create a polygon
  bathymetry from a raster bathymetry file
- [`geonorge_bathymetry()`](geonorge_bathymetry.md) : Open Geonorge
  bathymetry shapefiles
- [`clip_shapefile()`](clip_shapefile.md) : Clip a shapefile using a
  bounding area
- [`rotate_crs()`](rotate_crs.md) : Rotate CRS

## Size adjustment functions

These functions convert line widths and font sizes measured in points
(pt) to ggplot2 equivalents. They can be used to follow line width and
font size requirements set by journals.

- [`LS()`](LS.md) : Convert line sizes measured as points to ggplot line
  sizes
- [`FS()`](FS.md) : Convert font sizes measured as points to ggplot font
  sizes

## Map data

Shape- and raster files used by the package. Higher resolution files are
stored in the
[ggOceanMapsLargeData](https://github.com/MikkoVihtakari/ggOceanMapsLargeData)
repository. These files are automatically used by the basemap function.
The `basemap` function also supports custom sf and stars objects.

- [`dd_land`](dd_land.md) : Decimal degree land shapes
- [`dd_rbathy`](dd_rbathy.md) : Decimal degree bathymetry

## Datasets

The package contains ICES and Norwegian Directorate of Fisheries areas
as sf objects. These shapefiles are used in examples and included here
for convenience because they are regularly needed by the package author.

- [`ices_areas`](ices_areas.md) : ICES Advisory Areas
- [`fdir_main_areas`](fdir_main_areas.md) : Major fisheries areas
  (hovedomraade) of Norway
- [`fdir_sub_areas`](fdir_sub_areas.md) : Norwegian sub-areas (lokasjon)
  for commercial fishing

## Internal basemap functions

Internal functions dealing with data and graphics for basemap. These
functions are run on the background and only needed if you modify your
maps beyond the options offered by ggOceanMaps.

- [`basemap_data()`](basemap_data.md) : Create basemapData object for
  basemap plotting
- [`define_shapefiles()`](define_shapefiles.md) : Define a shapefile to
  use in plotting from the limits argument
- [`guess_coordinate_columns()`](guess_coordinate_columns.md) : Guess
  longitude and latitude columns in a data frame
- [`is_decimal_limit()`](is_decimal_limit.md) : Test whether a limit
  argument is specified as decimal degrees.
- [`map_cmd()`](map_cmd.md) : Return map elements for basemap
- [`round_any()`](round_any.md) : Round to multiple of any number
- [`quiet()`](quiet.md) : Return function output quietly
- [`dd_to_deg()`](dd_to_deg.md) : Convert decimal degree values to
  angular degrees
- [`deg_to_dd()`](deg_to_dd.md) : Convert angular degrees to decimal
  degree values
- [`select_element()`](select_element.md) : Select an element of each
  vector from a list
- [`dd_clip_boundary()`](dd_clip_boundary.md) : Create clip boundary
  from decimal degree limits
