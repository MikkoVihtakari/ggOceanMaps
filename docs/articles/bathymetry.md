# Bathymetry in ggOceanMaps

``` r

library(ggOceanMaps)
library(ggplot2)
```

ggOceanMaps can plot bathymetry from five different kinds of data
source. Each source has a sweet spot: some need nothing but the package
itself, others require an internet connection or a permanent download.
This vignette walks through all five in order of increasing setup cost.

## Quick decision guide

| Need | Recommended source | `bathy.style` |
|----|----|----|
| Anything, anywhere, no setup | Shipped low-res raster | `"rbb"` (default) |
| Higher detail, polar / global maps | ggOceanMapsLargeData | `"rcb"`, `"pb"`, `"cb"` |
| Your own GEBCO / ETOPO / IBCAO file | Local raster via `userpath` | `"rub"` |
| European waters, ~115 m | EMODnet WCS (live) | `"wemb"` |
| Anywhere on the globe, ~1.85 km | ETOPO1 WCS (live) | `"wceb"` |
| Custom contour polygons / land | [`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md) + [`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md) / [`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md) | `shapefiles = list(...)` |

Substitute `g` for the final `b` in any abbreviation to get the grey
variant (`rbb` → `rbg`, `wemb` → `wemg`, …). For the full alias list see
[`?basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md).

## 1. Shipped low-resolution raster (no setup)

The package bundles a coarse global bathymetry raster (`dd_rbathy`)
that’s always available. This is the default — just toggle
`bathymetry = TRUE`:

``` r

basemap(limits = c(-20, 30, 50, 70), bathymetry = TRUE)
```

![](bathymetry_files/figure-html/unnamed-chunk-2-1.png)

Or set the style explicitly:

``` r

basemap(c(11, 16, 67.3, 68.6), bathy.style = "rbb")
```

![](bathymetry_files/figure-html/unnamed-chunk-3-1.png)

Greyscale variant:

``` r

basemap(c(11, 16, 67.3, 68.6), bathy.style = "rbg")
```

![](bathymetry_files/figure-html/unnamed-chunk-4-1.png)

The shipped raster is intentionally coarse so the package stays under
CRAN’s size cap. It’s fine for overview maps and quick exploratory
plots; for publication maps you’ll usually want one of the higher-detail
options below.

## 2. ggOceanMapsLargeData (one-time download per region)

The companion repository
[ggOceanMapsLargeData](https://github.com/MikkoVihtakari/ggOceanMapsLargeData)
hosts higher-resolution rasters, polygon-contour bathymetries, and
contour lines for the supported regions (Decimal Degree, Arctic
Stereographic, Antarctic Stereographic, plus several pre-made regional
shapefile sets).
[`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
downloads what it needs on first use and caches it locally.

**One-time setup.** Pick a permanent directory (so files don’t vanish
between R sessions) and add this line to `~/.Rprofile`:

``` r

options(ggOceanMaps.datapath = "~/ggOceanMaps_data")
```

Then any high-res style will download into that directory and reuse it:

``` r

# Continuous raster — recommended for most maps
basemap(c(11, 16, 67.3, 68.6), bathy.style = "rcb")

# downsample = n speeds up rendering at the cost of detail
basemap(c(11, 16, 67.3, 68.6), bathy.style = "rcb", downsample = 10)

# Greyscale variant
basemap(c(11, 16, 67.3, 68.6), bathy.style = "rcg")

# Polygon contours (pre-2.0 default)
basemap(c(11, 16, 67.3, 68.6), bathy.style = "pb")
basemap(c(11, 16, 67.3, 68.6), bathy.style = "pg")

# Plain contour lines
basemap(c(11, 16, 67.3, 68.6), bathy.style = "cb")
basemap(c(11, 16, 67.3, 68.6), bathy.style = "cg")
```

On the first call a menu asks you to confirm the download (~15–100 MB
depending on the region). Subsequent calls are instant.

## 3. Your own raster (GEBCO, ETOPO, IBCAO, …)

Sometimes you want a specific dataset — the latest GEBCO grid, an ETOPO
2022 variant, a regional IBCAO compilation, or a custom-processed file
from your group. Download the file once, point ggOceanMaps at it through
`ggOceanMaps.userpath`, and use the `rub` / `rug` styles:

``` r

# Set once (in .Rprofile for persistence):
options(ggOceanMaps.userpath = "path/to/your/bathymetry.nc")

# Then any basemap call uses your file:
basemap(c(11, 16, 67.3, 68.6), bathy.style = "rub")
basemap(c(11, 16, 67.3, 68.6), bathy.style = "rub", downsample = 10)
basemap(c(11, 16, 67.3, 68.6), bathy.style = "rug")
```

Any file format
[`stars::read_stars()`](https://r-spatial.github.io/stars/reference/read_stars.html)
can open is accepted (NetCDF `.nc`, GeoTIFF `.tif`, GMT `.grd`, …). The
file is read fully every time, so the user-raster route can be slower
than a pre-processed ggOceanMapsLargeData object for the same region.
Use `downsample` to trade resolution for speed.

`ggOceanMaps.userpath` can also be used by
[`get_depth()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/get_depth.md)
to look up point depths from your raster:

``` r

dt <- data.frame(lon = seq(-20, 80, length.out = 41), lat = 50:90)
dt <- get_depth(dt, bathy.style = "ru")
qmap(dt, color = depth) + scale_color_viridis_c()
```

## 4. Live download from a Web Coverage Service (WCS)

For one-off maps or workflows where you don’t want to manage local
files, ggOceanMaps can fetch bathymetry on demand from two OGC Web
Coverage Services. No pre-download needed; downloaded tiles are cached
under `getOption("ggOceanMaps.datapath")` so subsequent calls for the
same bbox are instant.

| Source | Style | Coverage | Resolution |
|----|----|----|----|
| **EMODnet** | `wcs_emodnet_blues` (`wemb`) | European waters | ~115 m |
| **ETOPO1 (NOAA NCEI)** | `wcs_etopo_blues` (`wceb`) | Global | ~1.85 km |

``` r

# North Sea — high-resolution European waters from EMODnet
basemap(c(2, 3, 54, 55), bathy.style = "wemb")

# Hawaii — global coverage from ETOPO
basemap(c(-160, -154, 18, 23), bathy.style = "wceb")

# Indonesia / Java Trench — outside EMODnet, also ETOPO
basemap(c(110, 120, -20, 30), bathy.style = "wceb")
```

If you pick EMODnet for an area outside its European coverage, you get a
clear error pointing you to ETOPO:

``` r

basemap(c(110, 120, -20, 30), bathy.style = "wemb")
#> Error: Bounding box (110.0° to 120.0° lon, -20.0° to 30.0° lat) lies
#> entirely outside the approximate coverage of EMODnet (≈-36° to 43° lon,
#> 15° to 90° lat).
#> For global bathymetry coverage, download GEBCO or ETOPO data locally and
#> use raster_bathymetry() + vector_bathymetry() instead, or wait for a
#> global WCS source to be added to ggOceanMaps.
```

(Replace `wemb` with `wceb` and the same call works.)

### Manual fetch with `wcs_bathymetry()`

The `bathy.style` route is the simplest, but for full control — passing
the raster into
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md),
combining multiple regions, sharing a cache with other tools — call
[`wcs_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/wcs_bathymetry.md)
directly:

``` r

bathy <- wcs_bathymetry(c(2, 3, 54, 55), source = "emodnet")

basemap(c(2, 3, 54, 55),
        shapefiles = list(land = dd_land, glacier = NULL,
                          bathy = bathy$raster),
        bathymetry = TRUE)
```

### WCS caveats

- **Decimal-degree limits only.** Polar maps and projected-CRS limits
  are not supported (yet).
- **Per-source size caps.** EMODnet defaults to a 50 deg² maximum
  bounding box (it reads 8-byte doubles internally and a 4° tile already
  exceeds its read cap); ETOPO defaults to 2000 deg² because the
  underlying grid is much coarser. Override either with
  `max_area_deg2 = ...` in
  [`wcs_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/wcs_bathymetry.md).
  Larger boxes are tiled and mosaicked automatically.
- **Citation.** EMODnet is CC-BY
  (<https://emodnet.ec.europa.eu/en/bathymetry>); ETOPO1 is Amante &
  Eakins 2009, NOAA NGDC
  (<https://www.ncei.noaa.gov/products/etopo-global-relief-model>). Cite
  the source when publishing figures.

## 5. Build your own shapefiles

If you need contour polygons, your own depth bins, or a matched land +
bathymetry pair, the
[`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md)
/
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
/
[`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md)
pipeline turns any raster source into reusable shapefile objects:

``` r

# 1. Process the raster — crop, sign-flip, optionally bin into contours
rb <- raster_bathymetry(
  "path/to/your/bathymetry.nc",
  depths = c(50, 200, 500, 1000, 2000, 4000),  # depth break points
  boundary = c(-5, 10, 50, 60)                  # crop to your region
)

# 2a. Vectorize bathymetry into depth-band polygons
vb <- vector_bathymetry(rb, drop.crumbs = 10)   # drop islands < 10 km²

# 2b. Vectorize land from the same raster
vl <- vector_land(rb, drop.crumbs = 10)

# 3. Plot — both layers plug into basemap()
basemap(c(-5, 10, 50, 60),
        shapefiles = list(land = vl, glacier = NULL, bathy = vb),
        bathymetry = TRUE)
```

`depths = NULL` skips the binning step and gives you a continuous raster
— useful for `geom_raster`-style fills without vectorization overhead.

Save the processed objects so you don’t pay the processing cost again:

``` r

save(vb, vl, file = "my_region_bathy.rda")
```

## Picking a style at the bbox level

Switching `bathy.style` between maps in a multi-panel figure works as
expected; each panel uses its own source:

``` r

library(patchwork)
p1 <- basemap(c(2, 3, 54, 55), bathy.style = "wemb") + ggtitle("EMODnet (115 m)")
p2 <- basemap(c(-160, -154, 18, 23), bathy.style = "wceb") + ggtitle("ETOPO (1.85 km)")
p1 + p2
```

## See also

- [`?basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
  for the full `bathy.style` reference table.
- [`?wcs_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/wcs_bathymetry.md)
  for the live download function.
- [`?raster_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md),
  [`?vector_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md),
  [`?vector_land`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md)
  for the build-your-own pipeline.
- [`vignette("cookbook")`](https://mikkovihtakari.github.io/ggOceanMaps/articles/cookbook.md)
  for short copy-pasteable recipes.
- The user manual:
  [`vignette("ggOceanMaps")`](https://mikkovihtakari.github.io/ggOceanMaps/articles/ggOceanMaps.md).
