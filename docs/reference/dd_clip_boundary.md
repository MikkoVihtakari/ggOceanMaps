# Create clip boundary from decimal degree limits

Creates a clip boundary for map cropping from decimal degree limits
counter-clockwise following parallels

## Usage

``` r
dd_clip_boundary(limits, crs, expand.factor = NULL)
```

## Arguments

- limits:

  Numeric vector of length 4 giving the map limits in decimal degrees.
  See
  [basemap](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md).

- crs:

  Coordinate reference system in
  [`st_crs`](https://rdrr.io/pkg/sf/man/st_crs.html) format.

- expand.factor:

  Expansion factor for map limits. Set to `NULL` to ignore.

## Author

Mikko Vihtakari
