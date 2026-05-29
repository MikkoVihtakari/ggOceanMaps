# Clip a shapefile using a bounding area

Clips an area from a larger shapefile provided in sf or sp formats.

## Usage

``` r
clip_shapefile(
  x,
  limits,
  proj.limits = 4326,
  simplify = FALSE,
  tol = 60,
  return.boundary = FALSE,
  extra.validate = FALSE
)
```

## Arguments

- x:

  Original shapefile to be clipped as a an
  [sf](https://r-spatial.github.io/sf/reference/sf.html) or `sp`
  polygons object. Required. Must contain
  [`CRS`](https://r-spatial.github.io/sf/reference/st_crs.html)
  information.

- limits:

  The constraining area used to clip `x`. Required. Either a numeric
  vector of length 4 or a spatial object from the sf or sp packages. The
  first element of the numeric vector defines the minimum longitude,
  second element the maximum longitude, third element the minimum
  latitude and fourth element the maximum latitude of the bounding box.
  If a spatial object, it must contain
  [`CRS`](https://r-spatial.github.io/sf/reference/st_crs.html)
  information. See details.

- proj.limits:

  The [`CRS`](https://r-spatial.github.io/sf/reference/st_crs.html)
  projection attributes for `limits`. Hence format accepted by
  [`st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html) will
  suffice but integers are the easiest. Defaults to decimal degrees.

- simplify:

  Should the `x` geometry be simplified before clipping? Useful to make
  the function faster for large shape files. Uses
  [`st_simplify`](https://r-spatial.github.io/sf/reference/geos_unary.html)
  function.

- tol:

  Numerical tolerance value to be used for simplification. See
  `?sf::st_simplfy`.

- return.boundary:

  Logical. If `TRUE` returns the clip boundary together with the
  shapefile.

- extra.validate:

  Logical indicating whether `x` should be run through extra validation.
  Slows down the function but is necessary in some cases involving
  anti-meridian.

## Value

Clipped spatial object. If `return.boundary = TRUE`, a list containing
the shapefile together with the clip boundary.

## Details

The function uses the
[`st_intersection`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html)
function to clip smaller polygons from larger ones. The clip area is
constrained by either a numeric vector or a spatial object in the
`limits` argument. Defining `limits` by a
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object gives
greater freedom for the clip area as the area does not have to be
rectangular.

## See also

Other create shapefiles:
[`geonorge_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/geonorge_bathymetry.md),
[`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md),
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md),
[`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md),
[`wcs_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/wcs_bathymetry.md)

## Author

Mikko Vihtakari
