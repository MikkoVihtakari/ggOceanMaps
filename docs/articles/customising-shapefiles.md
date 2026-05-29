# Customising shapefiles

``` r

library(ggOceanMaps)
```

When the [premade
maps](https://mikkovihtakari.github.io/ggOceanMaps/articles/premade-maps.md)
do not cover your region at the resolution you need, you can supply your
own land, glacier, and bathymetry layers to
[`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md).
This article covers the four building blocks:

1.  The `shapefiles = list(...)` contract
    [`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
    expects.
2.  Clipping an existing layer to your region with
    [`clip_shapefile()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/clip_shapefile.md).
3.  Building bathymetry **and** matching land from a raster with
    [`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md)
    →
    [`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
    /
    [`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md).
4.  Reading Norwegian Geonorge depth data with
    [`geonorge_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/geonorge_bathymetry.md).

For *where the bathymetry data come from* (shipped,
ggOceanMapsLargeData, your own raster, or live WCS), see the [Bathymetry
article](https://mikkovihtakari.github.io/ggOceanMaps/articles/bathymetry.md).
This one is about turning data you already have into ggOceanMaps
shapefiles.

## The shapefiles argument

[`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
accepts a named list of three sf/stars objects:

``` r

basemap(
  limits = c(-5, 10, 50, 60),
  shapefiles = list(land = my_land, glacier = NULL, bathy = my_bathy),
  bathymetry = TRUE
)
```

- **land** — an sf polygon layer (required).
- **glacier** — an sf polygon layer, or `NULL` if you have none.
- **bathy** — either a `stars` raster (continuous/binned raster styles)
  or an sf polygon layer of depth contours (vector styles), or `NULL`.

All three must share **one projection**.
[`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
uses that CRS for the map, so there is no need to call
[`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
yourself.
[`shapefile_list()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/shapefile_list.md)
shows the same structure for the built-in sets and is a good template.

## Clipping an existing layer

[`clip_shapefile()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/clip_shapefile.md)
crops any sf polygon layer to a bounding box. It is the quickest way to
make a regional land layer from a larger one (e.g. the shipped
`dd_land`):

``` r

land <- clip_shapefile(
  dd_land,
  limits = c(-5, 10, 50, 60)   # c(xmin, xmax, ymin, ymax), decimal degrees
)

basemap(limits = c(-5, 10, 50, 60),
        shapefiles = list(land = land, glacier = NULL, bathy = NULL))
```

`limits` are interpreted with `proj.limits` (decimal degrees by
default). For a projected input layer, pass the limits in that
projection’s units and set `proj.limits` to its CRS, or keep `limits` in
degrees and let the function reproject the clip box. Use
`return.boundary = TRUE` to get the clipping polygon back for
inspection.

## Building land + bathymetry from a raster

A single source grid (GEBCO, ETOPO, IBCAO, …) gives you a matched land
and bathymetry pair. The pipeline is
[`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md)
first, then
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
and/or
[`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md):

``` r

# 1. Read + crop + sign-flip the grid; bin to depth contours.
rb <- raster_bathymetry(
  bathy = "path/to/your/bathymetry.nc",
  depths = c(50, 200, 500, 1000, 2000, 4000),  # contour break points
  proj.out = 4326,
  boundary = c(-5, 10, 50, 60)                  # crop early to save memory
)
```

[`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md)
returns a `bathyRaster`: a list with the processed `raster` (a `stars`
object) and the depth intervals. Feed that object to the vectorisers:

``` r

# 2a. Depth-contour polygons for the vector bathymetry styles
vb <- vector_bathymetry(rb, drop.crumbs = 10)   # drop polygons < 10 km^2

# 2b. Land polygons extracted from the same grid
vl <- vector_land(rb, drop.crumbs = 10)

# 3. Plug both into basemap()
basemap(
  limits = c(-5, 10, 50, 60),
  shapefiles = list(land = vl, glacier = NULL, bathy = vb),
  bathymetry = TRUE
)
```

[`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md)
takes the same `bathyRaster` as
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
and the same cleanup arguments (`drop.crumbs`, `remove.holes`,
`smooth`), so the land and bathymetry edges line up. To keep continuous
(unbinned) raster bathymetry instead of depth-contour polygons, pass
`depths = NULL` to
[`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md)
and use `rb$raster` directly as `bathy` (skipping
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)).

**Save the processed layers** so you do not re-run the heavy raster step
every session:

``` r

save(vb, vl, file = "north_sea_shapes.rda")
# later:
load("north_sea_shapes.rda")
```

## Reading Geonorge depth data

For Norwegian waters, the
[Geonorge](https://kartkatalog.geonorge.no/metadata/kartverket/dybdedata/2751aacf-5472-4850-a208-3532a51c529a)
depth-area product comes as `.gml`.
[`geonorge_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/geonorge_bathymetry.md)
reads it into a ready-to-use depth-polygon sf object:

``` r

gb <- geonorge_bathymetry("path/to/Dybdeareal.gml")

basemap(
  limits = c(10, 20, 68, 71),
  shapefiles = list(land = dd_land, glacier = NULL, bathy = gb),
  bathymetry = TRUE
)
```

By default it reads the `"dybdeareal"` layer; pass `layer =` if your
file uses a different layer name, and `verbose = TRUE` to see the
reading steps. The output keeps the file’s projection — make sure your
`land` layer is in the same CRS (reproject with
[`sf::st_transform()`](https://r-spatial.github.io/sf/reference/st_transform.html)
if needed).

## See also

- [Bathymetry](https://mikkovihtakari.github.io/ggOceanMaps/articles/bathymetry.md)
  — choosing and obtaining the underlying data.
- [Premade
  shapefiles](https://mikkovihtakari.github.io/ggOceanMaps/articles/premade-shapefiles.md)
  — how the built-in sets were built, as a fuller worked example.
- [Cookbook](https://mikkovihtakari.github.io/ggOceanMaps/articles/cookbook.md)
  — compact copy-pasteable versions of these recipes.
