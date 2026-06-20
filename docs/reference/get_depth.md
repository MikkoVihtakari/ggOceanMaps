# Extract depth for coordinates from a raster bathymetry dataset

Extracts depth from
[basemap](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
bathymetry raster dataset for coordinates in a data frame

## Usage

``` r
get_depth(
  data,
  bathy.style = "raster_continuous",
  lon = NULL,
  lat = NULL,
  shapefile = "DecimalDegree",
  proj.in = 4326,
  bind = TRUE,
  depth.col = "depth",
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame or [sf](https://rdrr.io/pkg/sf/man/sf.html) object
  containing geographic coordinates.

- bathy.style:

  Character defining the
  [basemap](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
  bathymetry raster which should be used for the depth extraction. Valid
  alternatives: `"raster_binned"` (or `"rb"`), `"raster_continuous"` (or
  `"rc"`; default), or `"raster_user"` (or `"ru"`).

- lon, lat:

  Either the names of the longitude and latitude columns in `data` or
  `NULL` to [guess the longitude and/or latitude
  columns](https://mikkovihtakari.github.io/ggOceanMaps/reference/guess_coordinate_columns.md)
  in `data`.

- shapefile:

  Land shape to which distances should be calculated. Either a character
  argument referring to a name of pre-made shapefiles in
  [`shapefile_list`](https://mikkovihtakari.github.io/ggOceanMaps/reference/shapefile_list.md),
  a single [sf](https://rdrr.io/pkg/sf/man/sf.html) or `sp` polygons
  object object or `NULL` to enable automatic definition of the land
  shapes based on `data`. Set to `"DecimalDegree"` by default which
  enables great circle distances using
  [s2](https://rdrr.io/pkg/sf/man/s2.html) features assuming a spherical
  Earth (as a contrast to earlier versions of the function which used
  flat Earth).

- proj.in:

  [`coordinate reference system`](https://rdrr.io/pkg/sf/man/st_crs.html)
  of `data`.

- bind:

  Logical indicating whether `x` should be returned with the distances
  (`TRUE`, default) or should the distances be returned as vector
  (`FALSE`).

- depth.col:

  The name of the depth column, if `bind = TRUE`. Defaults to "depth".

- verbose:

  Logical indicating whether information about the process should be
  returned as messages. Set to `FALSE` to make the function silent.

## Value

Returns a vector if `bind = FALSE`, otherwise a data frame. The depths
are given in a new column defined by the `dist.col` argument. The
distances are **kilometers**. `NA` distance means that the position is
on land.

## Details

Uses the [`st_extract`](https://rdrr.io/pkg/stars/man/st_extract.html)
function to extract values from
[basemap](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
bathymetry raster grids. Does not work for vector bathymetries.

## Author

Mikko Vihtakari

## Examples

``` r
if (FALSE) { # \dontrun{
dt <- data.frame(lon = seq(-20, 80, length.out = 41), lat = 50:90)
dt <- get_depth(dt)
qmap(dt, color = depth) + scale_color_viridis_c()
} # }
```
