# Pre-made shapefiles

``` r

library(ggOceanMaps)
library(sf)
library(stars)
```

This document records how the pre-made shapefiles bundled with — or
downloaded by — ggOceanMaps are produced, so the maps can be regenerated
or adapted. The rendered maps are shown in the [Premade maps
article](https://mikkovihtakari.github.io/ggOceanMaps/articles/premade-maps.html).

The low-resolution global shapefiles (`dd_land`, `dd_rbathy`) ship with
the package. Higher-resolution regional sets are stored in the
[ggOceanMapsLargeData](https://github.com/MikkoVihtakari/ggOceanMapsLargeData)
repository and downloaded automatically as needed.

> The code below is shown for reference and is **not run** when this
> article is built. It uses the current `sf`/`stars` toolchain. Earlier
> versions used `sp`/`rgeos`; those packages are retired and the
> equivalents here are
> [`sf::st_transform()`](https://rdrr.io/pkg/sf/man/st_transform.html)
> (reproject) and
> [`sf::st_make_valid()`](https://rdrr.io/pkg/sf/man/valid.html) (fix
> geometries).

## Setup

Download the source datasets (see [Data sources](#data-sources)) and
point the path objects at the folders on your computer.

``` r

etopoPath <- "path/to/etopo"
NEDPath   <- "path/to/naturalearth"
gebcoPath <- "path/to/gebco"
EEAPath   <- "path/to/eea"
outPath   <- "path/to/output"
```

Coordinate reference systems are taken straight from
[`shapefile_list()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/shapefile_list.md):

``` r

arcticCRS    <- sf::st_crs(shapefile_list("Arctic")$crs)
antarcticCRS <- sf::st_crs(shapefile_list("Antarctic")$crs)
ddCRS        <- sf::st_crs(shapefile_list("Decimal")$crs)
```

## Low-resolution global shapefiles

### Decimal degrees

Bathymetry — both the binned raster shipped with the package and a
continuous version:

``` r

# Binned raster bathymetry (shipped as dd_rbathy)
dd_rbathy <- raster_bathymetry(
  bathy = file.path(etopoPath, "ETOPO_2022_v1_60s_N90W180_surface.nc"),
  depths = c(50, 300, 500, 1000, 1500, 2000, 4000, 6000, 10000),
  proj.out = 4326,
  downsample = 1
)
save(dd_rbathy, file = file.path(outPath, "dd_rbathy.rda"), compress = "xz")

# Continuous raster bathymetry
dd_rbathy_cont <- raster_bathymetry(
  bathy = file.path(etopoPath, "ETOPO_2022_v1_60s_N90W180_surface.nc"),
  depths = NULL,
  proj.out = 4326,
  downsample = 1
)
save(dd_rbathy_cont, file = file.path(outPath, "dd_rbathy_cont.rda"), compress = "xz")
```

Land — Natural Earth land + minor islands. Natural Earth polygons
require the s2 engine to be turned off while clipping:

``` r

s2_mode <- sf::sf_use_s2()
suppressMessages(sf::sf_use_s2(FALSE))
on.exit(suppressMessages(sf::sf_use_s2(s2_mode)))

world   <- sf::st_read(file.path(NEDPath, "ne_10m_land/ne_10m_land.shp"))
islands <- sf::st_read(file.path(NEDPath, "ne_10m_minor_islands/ne_10m_minor_islands.shp"))

world   <- sf::st_make_valid(rbind(world, islands))
dd_land <- clip_shapefile(world, c(-180, 180, -90, 90))
all(sf::st_is_valid(dd_land))

save(dd_land, file = file.path(outPath, "dd_land.rda"), compress = "xz")
```

Glaciers — Natural Earth glaciated areas + Antarctic ice shelves:

``` r

glaciers    <- sf::st_read(file.path(NEDPath, "ne_10m_glaciated_areas/ne_10m_glaciated_areas.shp"))
iceshelves  <- sf::st_read(file.path(NEDPath, "ne_10m_antarctic_ice_shelves_polys/ne_10m_antarctic_ice_shelves_polys.shp"))

glaciers   <- sf::st_make_valid(rbind(glaciers, iceshelves))
dd_glacier <- clip_shapefile(glaciers, c(-180, 180, -90, 90))
all(sf::st_is_valid(dd_glacier))

save(dd_glacier, file = file.path(outPath, "dd_glacier.rda"), compress = "xz")
```

### Arctic stereographic

Vector and continuous bathymetry, reprojected to the Arctic CRS:

``` r

rb <- raster_bathymetry(
  bathy = file.path(etopoPath, "ETOPO_2022_v1_60s_N90W180_surface.nc"),
  depths = c(50, 300, 500, 1000, 1500, 2000, 4000, 6000, 10000),
  proj.out = arcticCRS,
  boundary = c(-180, 180, 1, 90),
  downsample = 1
)
arctic_bathy <- vector_bathymetry(rb)
save(arctic_bathy, file = file.path(outPath, "arctic_bathy.rda"), compress = "xz")
```

Land and glaciers — reproject the global layers with
[`sf::st_transform()`](https://rdrr.io/pkg/sf/man/st_transform.html) and
repair geometries with
[`sf::st_make_valid()`](https://rdrr.io/pkg/sf/man/valid.html):

``` r

arctic_land <- clip_shapefile(world, c(-180, 180, 1, 90))
arctic_land <- sf::st_make_valid(sf::st_transform(arctic_land, arcticCRS))
save(arctic_land, file = file.path(outPath, "arctic_land.rda"), compress = "xz")

arctic_glacier <- clip_shapefile(glaciers, c(-180, 180, 40, 90))
arctic_glacier <- sf::st_make_valid(sf::st_transform(arctic_glacier, arcticCRS))
save(arctic_glacier, file = file.path(outPath, "arctic_glacier.rda"), compress = "xz")
```

### Antarctic stereographic

``` r

rb <- raster_bathymetry(
  bathy = file.path(etopoPath, "ETOPO_2022_v1_60s_N90W180_surface.nc"),
  depths = c(50, 300, 500, 1000, 1500, 2000, 4000, 6000, 10000),
  proj.out = antarcticCRS,
  boundary = c(-180, 180, -80, -30),
  downsample = 1
)
antarctic_bathy <- vector_bathymetry(rb)
save(antarctic_bathy, file = file.path(outPath, "antarctic_bathy.rda"), compress = "xz")

antarctic_land <- clip_shapefile(world, c(-180, 180, -90, -30))
antarctic_land <- sf::st_make_valid(sf::st_transform(antarctic_land, antarcticCRS))
save(antarctic_land, file = file.path(outPath, "antarctic_land.rda"), compress = "xz")

antarctic_glacier <- clip_shapefile(glaciers, c(-180, 180, -90, -30))
antarctic_glacier <- sf::st_make_valid(sf::st_transform(antarctic_glacier, antarcticCRS))
save(antarctic_glacier, file = file.path(outPath, "antarctic_glacier.rda"), compress = "xz")
```

## High-resolution regional shapefiles (ggOceanMapsLargeData)

### Europe

Download the EEA coastline from
<https://www.eea.europa.eu/en/datahub/datahubitem-view/af40333f-9e94-4926-a4f0-0a787f1d2b8f>,
unzip it, and point `EEAPath` at the folder.

``` r

europe_land <- sf::read_sf(file.path(EEAPath, "EEA_Coastline_20170228.shp"))
europe_land <- sf::st_make_valid(europe_land)
all(sf::st_is_valid(europe_land))

save(europe_land,
     file = file.path(outPath, "europe_land.rda"),
     compress = "xz")
```

### Svalbard

The Svalbard land, glacier, and bathymetry layers (EPSG:32633) follow
the same pattern: clip the source rasters/polygons to the Svalbard
extent (`shapefile_list("Svalbard")$limits`), reproject with
[`sf::st_transform()`](https://rdrr.io/pkg/sf/man/st_transform.html),
and
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
the binned raster. They originate from the
[PlotSvalbard](https://github.com/MikkoVihtakari/PlotSvalbard) package.

## Data sources

The data used by the package are not the property of the Institute of
Marine Research nor the package author. Please cite the data sources
used in any map you generate. See the [data-source
list](https://mikkovihtakari.github.io/ggOceanMaps/#citations-and-data-sources)
for the references.
