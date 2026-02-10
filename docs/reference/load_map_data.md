# Load large shapefile objects

Internal function to load large shapefile objects. Downloads the files
if they are not found `getOption("ggOceanMaps.datapath")`

## Usage

``` r
load_map_data(x, force = FALSE, downsample = 0)
```

## Arguments

- x:

  An object from [`shapefile_list`](shapefile_list.md).

- force:

  Logical indicating whether to download the file even though it exists.
  Useful when files in the [Github repository have been
  changed](https://github.com/MikkoVihtakari/ggOceanMapsLargeData).
  Overwrites the old file.

- downsample:

  Integer defining the downsampling rate for raster bathymetries. A
  value of 0 (default) does not downsample, 1 skips every second row, 2
  every second and third. See
  [`geom_stars`](https://r-spatial.github.io/stars/reference/geom_stars.html)

## Value

A list of spatial objects

## Details

This is an internal function, which is automatically run by the
[`basemap`](basemap.md) function. Common users do not need to worry
about these details.

## See also

[`basemap`](basemap.md)

## Author

Mikko Vihtakari
