# A list of pre-made shapefiles for `basemap`

Lists available pre-made shapefiles for plotting in the
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
function. Gives also instructions how to make custom ones.

## Usage

``` r
shapefile_list(name, get.data = FALSE)
```

## Arguments

- name:

  A character argument giving the name of a pre-made shapefile. Will be
  partially matched. Use "all" to list all available ones.

- get.data:

  Logical indicating whether spatial data should be returned instead of
  names of spatial data objects.

## Value

Returns a data frame of provided pre-made shapefiles, if `name = "all"`.
Returns a shapefile list containing the information for a particular map
otherwise.

## Details

Custom shapefiles for
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
should be defined as lists with (at least) following names (everything
should be provided as characters):

- **land** Name of the object containing land polygons. Required.

- **glacier** Name of the object containing glacier polygons. Use `NULL`
  if glaciers are not needed.

- **bathy** Name of the object containing land polygons. Use `NULL` if
  bathymetry is not needed.

All linked spatial data objects must be in same projection.
High-resolution pre-made data are still under development and may not be
available. Pre-made shapefiles contain additional elements that are used
in the
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
function, but not required for custom shapefile datasets.

## See also

Other basemap functions:
[`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md),
[`qmap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/qmap.md),
[`transform_coord()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/transform_coord.md)

## Author

Mikko Vihtakari

## Examples

``` r
shapefile_list("all")
#>                     name
#> 1    ArcticStereographic
#> 2 AntarcticStereographic
#> 3          DecimalDegree
#> 4               Svalbard
#> 5                 Europe
#>                                                                          land
#> 1    /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/arctic_land
#> 2 /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/antarctic_land
#> 3                                                                     dd_land
#> 4  /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/svalbard_land
#> 5    /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/europe_land
#>                                                                          glacier
#> 1    /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/arctic_glacier
#> 2 /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/antarctic_glacier
#> 3        /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/dd_glacier
#> 4  /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/svalbard_glacier
#> 5                                                                           <NA>
#>                                                                                                                                                                bathy
#> 1    dd_rbathy|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/dd_rbathy_cont|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/arctic_bathy
#> 2 dd_rbathy|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/dd_rbathy_cont|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/antarctic_bathy
#> 3        dd_rbathy|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/dd_rbathy_cont|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/dd_bathy
#> 4  dd_rbathy|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/dd_rbathy_cont|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/svalbard_bathy
#> 5        dd_rbathy|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/dd_rbathy_cont|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/dd_bathy
#>     crs                                      limits
#> 1  3995                                       c(30)
#> 2  3031                                      c(-35)
#> 3  4326                       c(-180, 180, -90, 90)
#> 4 32633 c(402204.7, 845943.9, 8253526.1, 8978517.5)
#> 5  3035        c(943609, 7601958, -375446, 6825119)
#>                                                                      path
#> 1 https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/
#> 2 https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/
#> 3 https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/
#> 4 https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/
#> 5 https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/
shapefile_list("Arctic") # partial matching
#> $name
#> [1] "ArcticStereographic"
#> 
#> $land
#> [1] "/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/arctic_land"
#> 
#> $glacier
#> [1] "/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/arctic_glacier"
#> 
#> $bathy
#>                                                                 raster_binned 
#>                                                                   "dd_rbathy" 
#>                                                             raster_continuous 
#> "/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/dd_rbathy_cont" 
#>                                                                        vector 
#>   "/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//RtmppcDrFF/arctic_bathy" 
#> 
#> $crs
#> [1] 3995
#> 
#> $limits
#> [1] 30
#> 
#> $path
#>                                                      ggOceanMapsLargeData 
#> "https://github.com/MikkoVihtakari/ggOceanMapsLargeData/raw/master/data/" 
#> 
```
