# Premade maps

## Overview

ggOceanMaps plots maps from **pre-made shapefiles**. Most of the time
you do not need to think about them:
[`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
picks the right set automatically from the `limits` (or `data`) you give
it. This article shows the maps that ship with — or are downloadable for
— the package, and the projection each uses.

The available sets are listed by
[`shapefile_list()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/shapefile_list.md):

``` r

shapefile_list("all")
#>                     name
#> 1    ArcticStereographic
#> 2 AntarcticStereographic
#> 3          DecimalDegree
#> 4               Svalbard
#> 5                 Europe
#>                                                                          land
#> 1    /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/arctic_land
#> 2 /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/antarctic_land
#> 3                                                                     dd_land
#> 4  /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/svalbard_land
#> 5    /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/europe_land
#>                                                                          glacier
#> 1    /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/arctic_glacier
#> 2 /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/antarctic_glacier
#> 3        /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/dd_glacier
#> 4  /var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/svalbard_glacier
#> 5                                                                           <NA>
#>                                                                                                                                                                bathy
#> 1    dd_rbathy|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/dd_rbathy_cont|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/arctic_bathy
#> 2 dd_rbathy|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/dd_rbathy_cont|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/antarctic_bathy
#> 3        dd_rbathy|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/dd_rbathy_cont|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/dd_bathy
#> 4  dd_rbathy|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/dd_rbathy_cont|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/svalbard_bathy
#> 5        dd_rbathy|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/dd_rbathy_cont|/var/folders/9j/t7m30trx0s33zy79x20y3wyh0000gn/T//Rtmp4uvJqL/dd_bathy
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
```

Each row is a named shapefile set with a coordinate reference system
(`crs`), a default extent (`limits`), and the names of its land /
glacier / bathymetry objects. You can force a particular set with the
first argument or the `shapefiles` argument (partial matching works, so
`"Arctic"` resolves to `"ArcticStereographic"`):

``` r

basemap(shapefiles = "Svalbard", bathymetry = TRUE)
```

Low-resolution global data (`dd_land`, `dd_rbathy`) ship with the
package. The higher-resolution regional sets live in the
[ggOceanMapsLargeData](https://github.com/MikkoVihtakari/ggOceanMapsLargeData)
repository and are **downloaded automatically on first use** to
`getOption("ggOceanMaps.datapath")`. Set that option to a permanent
folder in your `.Rprofile` so the files are reused across sessions;
otherwise they land in
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) and are
re-downloaded each session.

## Standard maps (selected automatically)

These three sets cover the whole globe and are chosen automatically from
the latitude range of your `limits`: polar-stereographic projections
near the poles, decimal degrees elsewhere. See the [user
manual](https://mikkovihtakari.github.io/ggOceanMaps/articles/ggOceanMaps.md)
for the exact selection rule.

### Decimal degree

The default map uses decimal degrees (EPSG:4326) and [Natural
Earth](https://www.naturalearthdata.com/downloads/10m-physical-vectors/)
1:10m land and minor-island polygons.

``` r

basemap("DecimalDegree", bathymetry = TRUE, glaciers = TRUE)
```

![Global basemap in decimal degrees (EPSG:4326) with land, glaciers, and
binned
bathymetry.](https://raw.githubusercontent.com/MikkoVihtakari/ggOceanMapsLargeData/master/docs/DecimalDegree.png)

Global basemap in decimal degrees (EPSG:4326) with land, glaciers, and
binned bathymetry.

### Arctic Stereographic

Used automatically when the map sits in the high north (EPSG:3995). Land
and glacier polygons and vector bathymetry are downloaded from
ggOceanMapsLargeData on first use.

``` r

basemap("ArcticStereographic", bathymetry = TRUE, glaciers = TRUE)
```

![Arctic stereographic basemap (EPSG:3995) with land, glaciers, and
bathymetry.](https://raw.githubusercontent.com/MikkoVihtakari/ggOceanMapsLargeData/master/docs/ArcticStereographic.png)

Arctic stereographic basemap (EPSG:3995) with land, glaciers, and
bathymetry.

### Antarctic Stereographic

The southern equivalent (EPSG:3031).

``` r

basemap("AntarcticStereographic", bathymetry = TRUE, glaciers = TRUE)
```

![Antarctic stereographic basemap (EPSG:3031) with land, glaciers, and
bathymetry.](https://raw.githubusercontent.com/MikkoVihtakari/ggOceanMapsLargeData/master/docs/AntarcticStereographic.png)

Antarctic stereographic basemap (EPSG:3031) with land, glaciers, and
bathymetry.

## Detailed regional maps

Higher-resolution sets for regions the package author works in. They are
downloaded from ggOceanMapsLargeData when first requested. The detailed
shapefiles can be **large**: find your limits with the standard maps
first, and export to a file rather than plotting into the screen device
for big extents.

### Svalbard

A detailed Svalbard map (EPSG:32633), originally from the
[PlotSvalbard](https://github.com/MikkoVihtakari/PlotSvalbard) package:

``` r

basemap("Svalbard", bathymetry = TRUE, glaciers = TRUE)
```

![The function asks to download the data the first time you use
it.](https://raw.githubusercontent.com/MikkoVihtakari/ggOceanMapsLargeData/master/docs/Svalbard.png)

The function asks to download the data the first time you use it.

Zooming to Kongsfjorden:

``` r

basemap(limits = c(10.9, 12.65, 78.83, 79.12),
        bathymetry = TRUE, shapefiles = "Svalbard",
        legends = FALSE, glaciers = TRUE)
```

![High-resolution map of Kongsfjorden, Svalbard, with bathymetry and
glaciers.](https://raw.githubusercontent.com/MikkoVihtakari/ggOceanMapsLargeData/master/docs/Kongsfjorden.png)

High-resolution map of Kongsfjorden, Svalbard, with bathymetry and
glaciers.

### Europe

A detailed European coastline (EPSG:3035) based on the [EEA
coastline](https://www.eea.europa.eu/en/datahub/datahubitem-view/af40333f-9e94-4926-a4f0-0a787f1d2b8f).
Useful for coastal maps around the European seas.

``` r

basemap(shapefiles = "Europe", limits = c(-15, 30, 40, 65), bathymetry = TRUE)
```

The Europe land polygons are downloaded from ggOceanMapsLargeData on
first use.

## Citing the data

The data used by the package are not the property of the Institute of
Marine Research nor the package author. Please cite the data sources
used in any map you generate. See the [data-source
list](https://mikkovihtakari.github.io/ggOceanMaps/#citations-and-data-sources)
for the references.
