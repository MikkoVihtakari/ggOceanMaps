# Define a shapefile to use in plotting from the limits argument

An internal function to make
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
code more readable

## Usage

``` r
define_shapefiles(limits, force_dd = FALSE)
```

## Arguments

- limits:

  A numeric vector of length 4: The first element defines the minimum
  longitude, the second element the maximum longitude, the third element
  the minimum latitude and the fourth element the maximum latitude of
  the bounding box.

- force_dd:

  Logical indicating whether the shapefile should be forced to
  DecimalDegree. Required for transforming dd\_\* shapefile objects to
  other projections.

## Value

A list containing the correct shapefile, a logical statement whether the
limits were supplied as decimal degrees and coordinate reference system.

## Details

This is an internal function, which is automatically run by the
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
function.

## See also

[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)

## Author

Mikko Vihtakari
