# Transform spatial coordinates to another projection

Transforms spatial coordinates from original projection (decimal degrees
assumed) to another projection.

## Usage

``` r
transform_coord(
  x = NULL,
  lon = NULL,
  lat = NULL,
  new.names = "auto",
  rotate = FALSE,
  proj.in = 4326,
  proj.out = NULL,
  verbose = FALSE,
  bind = FALSE,
  na = "ignore"
)
```

## Arguments

- x:

  Data frame to be transformed. Can be omitted if numeric vectors are
  assigned to `lon` and `lat`.

- lon, lat:

  Either a name of the longitude and latitude columns in `x` or a
  numeric vector containing longitude and latitude coordinates. Use
  `NULL` to [guess the longitude and/or latitude
  columns](https://mikkovihtakari.github.io/ggOceanMaps/reference/guess_coordinate_columns.md)
  in `x`.

- new.names:

  Character vector of length 2 specifying the names of transformed
  longitude and latitude columns, respectively. Alternatively `NULL`,
  which returns column names from `x` or "auto", which uses `NULL` if
  `bind = FALSE` and `c("lon.proj", "lat.proj")` if `bind = TRUE`.

- rotate:

  Logical indicating whether the projected maps should be rotated to
  point towards the pole relative to the mid-longitude limit.

- proj.in:

  The original
  [`CRS`](https://r-spatial.github.io/sf/reference/st_crs.html). If
  `NULL`, the projection is taken from `x`. `x` must be a
  [spatial](https://r-spatial.github.io/sf/reference/st.html) object in
  that case.

- proj.out:

  Character. Either `NULL`,
  [`CRS`](https://r-spatial.github.io/sf/reference/st_crs.html) the
  coordinates should be transformed to or a name of shapefiles in
  [`shapefile_list`](https://mikkovihtakari.github.io/ggOceanMaps/reference/shapefile_list.md).
  If `NULL`, the output projection will be automatically determined from
  data. This option requires decimal degrees as input option.

- verbose:

  Logical indicating whether information about the projection should be
  returned as message. Set to `FALSE` to make the function silent.

- bind:

  logical. Should only transformed coordinates be returned (`FALSE`,
  default) or should x be returned with transformed coordinates
  (`TRUE`)?

- na:

  character specifying the NA action for missing coordinates. The
  "ignore" option ignores the coordinates and returns NAs to transformed
  coordinates. The "remove" option removes missing values from `x`
  returning a message while doing it. Any other character argument will
  trigger `na.fail` stopping the function in case of missing
  coordinates.

## Value

Returns a data frame with transformed spatial coordinates.

## Details

If `x` is specified, the function guesses longitude and latitude columns
from `x` by default.

## See also

Other basemap functions:
[`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md),
[`qmap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/qmap.md),
[`shapefile_list()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/shapefile_list.md)

## Author

Mikko Vihtakari

## Examples

``` r
# Coordinates are automatically transformed to the pre-made shapefile
# projections:
x <- data.frame(lon = c(-150, 150), lat = c(60, 90))
transform_coord(x)
#>        lon     lat
#> 1 -1666567 2886579
#> 2        0       0
transform_coord(x, bind = TRUE)
#>    lon lat lon.proj lat.proj
#> 1 -150  60 -1666567  2886579
#> 2  150  90        0        0

x <- data.frame(lon = c(-150, 150), lat = c(20, 50))
transform_coord(x, bind = TRUE) # no transformation required.
#>    lon lat lon.proj lat.proj
#> 1 -150  20     -150       20
#> 2  150  50      150       50
```
