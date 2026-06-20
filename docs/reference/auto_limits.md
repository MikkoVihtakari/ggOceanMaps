# Automatic limits for basemap

Find limits for a
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
from a data frame.

## Usage

``` r
auto_limits(
  data,
  lon = NULL,
  lat = NULL,
  proj.in = 4326,
  proj.out = NULL,
  expand.factor = NULL,
  verbose = FALSE
)
```

## Arguments

- data:

  Data frame or a spatial object containing data for which the limits
  should be calculated.

- lon, lat:

  Names of longitude and latitude columns in `data` as character or
  integer index. If `NULL`, the column names are
  [guessed](https://mikkovihtakari.github.io/ggOceanMaps/reference/guess_coordinate_columns.md).

- proj.in:

  Original [`CRS`](https://rdrr.io/pkg/sf/man/st_crs.html) projection.
  Must be defined as character argument.

- proj.out:

  Resulting map projection. See
  [`transform_coord`](https://mikkovihtakari.github.io/ggOceanMaps/reference/transform_coord.md).

- expand.factor:

  Expansion factor for map limits. Set to `NULL` to ignore.

- verbose:

  Logical indicating whether information about the projection and
  guessed column names should be returned as message. Set to `FALSE` to
  make the function silent.

## Value

A list of limits and projections in `proj.in` and `proj.out` formats.

## Details

This is an internal function, which is automatically run by the
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
function.

## See also

Other customize shapefiles:
[`reorder_layers()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/reorder_layers.md),
[`theme_map()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/theme_map.md)

## Author

Mikko Vihtakari

## Examples

``` r
auto_limits(data = expand.grid(lon = c(-120, 180, 120),
   lat = c(60, 60, 80)))
#> $ddLimits
#>       xmin       xmax       ymin       ymax 
#> -139.10661  139.10661   50.94316   90.00000 
#> 
#> $projLimits
#>       xmin       xmax       ymin       ymax 
#> -2886578.7  2886578.7   544589.7  3333134.0 
#> 
#> $projBound
#> Geometry set for 1 feature 
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -2886579 ymin: 544589.7 xmax: 2886579 ymax: 3333134
#> Projected CRS: WGS 84 / Arctic Polar Stereographic
#> POLYGON ((-2886579 544589.7, -2886579 3333134, ...
#> 
#> $proj.in
#> Coordinate Reference System:
#>   User input: EPSG:4326 
#>   wkt:
#> GEOGCRS["WGS 84",
#>     ENSEMBLE["World Geodetic System 1984 ensemble",
#>         MEMBER["World Geodetic System 1984 (Transit)"],
#>         MEMBER["World Geodetic System 1984 (G730)"],
#>         MEMBER["World Geodetic System 1984 (G873)"],
#>         MEMBER["World Geodetic System 1984 (G1150)"],
#>         MEMBER["World Geodetic System 1984 (G1674)"],
#>         MEMBER["World Geodetic System 1984 (G1762)"],
#>         MEMBER["World Geodetic System 1984 (G2139)"],
#>         MEMBER["World Geodetic System 1984 (G2296)"],
#>         ELLIPSOID["WGS 84",6378137,298.257223563,
#>             LENGTHUNIT["metre",1]],
#>         ENSEMBLEACCURACY[2.0]],
#>     PRIMEM["Greenwich",0,
#>         ANGLEUNIT["degree",0.0174532925199433]],
#>     CS[ellipsoidal,2],
#>         AXIS["geodetic latitude (Lat)",north,
#>             ORDER[1],
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>         AXIS["geodetic longitude (Lon)",east,
#>             ORDER[2],
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>     USAGE[
#>         SCOPE["Horizontal component of 3D system."],
#>         AREA["World."],
#>         BBOX[-90,-180,90,180]],
#>     ID["EPSG",4326]]
#> 
#> $proj.out
#> Coordinate Reference System:
#>   User input: EPSG:3995 
#>   wkt:
#> PROJCRS["WGS 84 / Arctic Polar Stereographic",
#>     BASEGEOGCRS["WGS 84",
#>         ENSEMBLE["World Geodetic System 1984 ensemble",
#>             MEMBER["World Geodetic System 1984 (Transit)"],
#>             MEMBER["World Geodetic System 1984 (G730)"],
#>             MEMBER["World Geodetic System 1984 (G873)"],
#>             MEMBER["World Geodetic System 1984 (G1150)"],
#>             MEMBER["World Geodetic System 1984 (G1674)"],
#>             MEMBER["World Geodetic System 1984 (G1762)"],
#>             MEMBER["World Geodetic System 1984 (G2139)"],
#>             MEMBER["World Geodetic System 1984 (G2296)"],
#>             ELLIPSOID["WGS 84",6378137,298.257223563,
#>                 LENGTHUNIT["metre",1]],
#>             ENSEMBLEACCURACY[2.0]],
#>         PRIMEM["Greenwich",0,
#>             ANGLEUNIT["degree",0.0174532925199433]],
#>         ID["EPSG",4326]],
#>     CONVERSION["Arctic Polar Stereographic",
#>         METHOD["Polar Stereographic (variant B)",
#>             ID["EPSG",9829]],
#>         PARAMETER["Latitude of standard parallel",71,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8832]],
#>         PARAMETER["Longitude of origin",0,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8833]],
#>         PARAMETER["False easting",0,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8806]],
#>         PARAMETER["False northing",0,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8807]]],
#>     CS[Cartesian,2],
#>         AXIS["easting (X)",south,
#>             MERIDIAN[90,
#>                 ANGLEUNIT["degree",0.0174532925199433]],
#>             ORDER[1],
#>             LENGTHUNIT["metre",1]],
#>         AXIS["northing (Y)",south,
#>             MERIDIAN[180,
#>                 ANGLEUNIT["degree",0.0174532925199433]],
#>             ORDER[2],
#>             LENGTHUNIT["metre",1]],
#>     USAGE[
#>         SCOPE["Polar research."],
#>         AREA["Northern hemisphere - north of 60°N onshore and offshore, including Arctic."],
#>         BBOX[60,-180,90,180]],
#>     ID["EPSG",3995]]
#> 
```
