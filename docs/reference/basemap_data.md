# Create basemapData object for basemap plotting

Internal function to create a `basemapData` object for
[`basemap`](basemap.md)

## Usage

``` r
basemap_data(
  limits = NULL,
  data = NULL,
  shapefiles = NULL,
  crs = NULL,
  bathymetry = FALSE,
  bathy.type = NULL,
  downsample = 0,
  glaciers = FALSE,
  lon.interval = NULL,
  lat.interval = NULL,
  expand.factor = 1.1,
  rotate = FALSE,
  verbose = FALSE
)
```

## Arguments

- limits:

  Map limits. One of the following:

  - **numeric vector** of length 4: The first element defines the start
    longitude, the second element the end longitude (counter-clockwise),
    the third element the minimum latitude, and the fourth element the
    maximum latitude of the bounding box. Also accepts
    [`sf::st_bbox`](https://r-spatial.github.io/sf/reference/st_bbox.html)
    type named vectors with limits in any order. The coordinates can be
    given as decimal degrees or coordinate units for shapefiles used by
    a projected map. Produces a rectangular map. Latitude limits not
    given in min-max order are automatically ordered to respect this
    requirement.

  - **single integer** between 30 and 88 or -88 and -30 produces a polar
    map for the Arctic or Antarctic, respectively.

  Can be omitted if `data` or `shapefiles` are defined.

- data:

  A data frame, sp, or
  [sf](https://r-spatial.github.io/sf/reference/sf.html) shape
  containing longitude and latitude coordinates. If a data frame, the
  coordinates have to be given in decimal degrees. The limits are
  extracted from these coordinates and produce a rectangular map. Suited
  for situations where a certain dataset is plotted on a map. The
  function attempts to [guess the correct
  columns](guess_coordinate_columns.md) and it is advised to use
  intuitive column names for longitude (such as "lon", "long", or
  "longitude") and latitude ("lat", "latitude") columns. Can be omitted
  if `limits` or `shapefiles` are defined.

- shapefiles:

  Either a [list containing shapefile information](shapefile_list.md) or
  a character argument referring to a name of pre-made shapefiles in
  [`shapefile_list`](shapefile_list.md). This name is partially matched.
  Can be omitted if `limits` or `data` is defined as decimal degrees.

- crs:

  [Coordinate reference
  system](https://r-spatial.github.io/sf/reference/st_crs.html) (CRS)
  for the map. If `NULL` (default), the CRS is selected automatically
  based on `limits`, `data`, or `shapefiles`. Passed to
  [`st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html).
  Typically integers giving the EPGS code are the easiest. Cannot be
  used simultaneously with `rotate`.

- bathymetry:

  Logical indicating whether bathymetry should be added to the map.
  Functions together with `bathy.style`. See Details.

- downsample:

  Integer defining the downsampling rate for raster bathymetries. A
  value of 0 (default) does not downsample, 1 skips every second row, 2
  every second and third. See
  [`geom_stars`](https://r-spatial.github.io/stars/reference/geom_stars.html)

- glaciers:

  Logical indicating whether glaciers and ice sheets should be added to
  the map.

- lon.interval, lat.interval:

  Numeric value specifying the interval of longitude and latitude grids.
  `NULL` finds reasonable defaults depending on `limits`.

- expand.factor:

  Expansion factor for map limits. Can be used to zoom in (decrease the
  value under 1) and out (increase the value over 1) automatically
  (`data`) limited maps. Defaults to 1, which means that outermost data
  points are located at the boundaries of the plotting region.

- rotate:

  Logical indicating whether the projected maps should be rotated to
  point towards the pole relative to the mid-longitude limit.

- verbose:

  Logical indicating whether information about the projection and
  guessed column names should be returned as messages. Set to `FALSE` to
  make the function silent.

## Value

A list of class `basemapData` containing information required for
plotting a [`basemap`](basemap.md).

## Details

This is an internal function, which is automatically run by the
[`basemap`](basemap.md) function. Common users do not need to worry
about these details.

## See also

[`basemap`](basemap.md)

## Author

Mikko Vihtakari
