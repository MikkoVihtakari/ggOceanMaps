# Download bathymetry from a WCS endpoint

Fetches gridded bathymetry data on demand from an OGC Web Coverage
Service (WCS) and returns it as a `bathyRaster` object compatible with
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
and
[`vector_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md).
Two sources are currently supported: `"emodnet"` (~115 m European
waters) and `"etopo"` (1 arc-minute / ~1.85 km global).

## Usage

``` r
wcs_bathymetry(
  limits,
  source = "emodnet",
  coverage = NULL,
  cache_dir = NULL,
  force = FALSE,
  max_area_deg2 = NULL,
  tile_size_deg = NULL,
  downsample = 0,
  timeout = 60,
  verbose = TRUE
)
```

## Arguments

- limits:

  Numeric vector of length 4 giving the bounding box in decimal degrees
  as `c(xmin, xmax, ymin, ymax)`.

- source:

  Character. The WCS source to query. One of `"emodnet"` (European
  waters, high-res) or `"etopo"` (global, 1 arc-minute). Partial
  matching applies.

- coverage:

  Character. Override the default coverage for the source (e.g.
  `"emodnet__mean_2022"` for the 2022-vintage EMODnet DTM). `NULL` uses
  the source default.

- cache_dir:

  Character. Directory in which downloaded GeoTIFFs are cached. Defaults
  to `getOption("ggOceanMaps.datapath")` or
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- force:

  Logical. If `TRUE`, re-download even when a cached file exists.

- max_area_deg2:

  Numeric. Maximum bounding-box area (in degree-squared) allowed before
  the function errors. Guards against accidentally downloading a very
  large area. `NULL` uses the source default (50 for EMODnet, 2000 for
  ETOPO). Bounding boxes larger than the source's single-request cap are
  split into tiles and mosaicked automatically – see Details.

- tile_size_deg:

  Numeric. Edge length (in degrees) of the largest single-request tile.
  `NULL` uses the source default (3 for EMODnet, 30 for ETOPO). EMODnet
  reads 8-byte doubles internally so a 4-degree tile already exceeds its
  ~98 MB read cap; ETOPO is much coarser so larger tiles are fine.

- downsample:

  Non-negative integer. Number of grid cells to skip when requesting the
  raster. `0` (default) keeps the source resolution; `1` requests half
  the rows and columns; `n` requests every `(n+1)`-th cell. Downsampling
  is performed by the WCS server, reducing both transfer size and memory
  use.

- timeout:

  Positive numeric. Minimum HTTP timeout in seconds. A larger existing
  global `timeout` option is respected.

- verbose:

  Logical. Print download progress and informational messages.

## Value

A `bathyRaster` object: a list with elements `raster` (a
[`read_stars`](https://rdrr.io/pkg/stars/man/read_stars.html) object
with positive depth values) and `depth.invervals` (a length-2 numeric
range).

## Details

**EMODnet** serves the European-waters bathymetric DTM at ~115 m native
resolution (~0.00104 deg) in EPSG:4326 GeoTIFF format. The 1x1 deg tile
around the North Sea is ~4 MB; a 5x5 deg tile would be ~100 MB and hit
the server's ~98 MB read cap. Coverage is European regional seas only
(~-36 to 43 lon, ~15 to 90 lat).

**ETOPO** (ETOPO1 Ice Surface, served by NOAA NCEI) is a global 1
arc-minute (~1.85 km) topo-bathy grid in EPSG:4326. Useful when EMODnet
has no coverage. NCEI returns the GeoTIFF inside a multipart/related
MIME envelope; `wcs_bathymetry()` extracts the binary part
transparently.

To handle bounding boxes larger than the source's single-request cap,
`wcs_bathymetry()` splits the request into tiles of at most
`tile_size_deg` per axis, caches each tile, and mosaicks them via a GDAL
virtual raster. Each tile is cached independently so subsequent
overlapping requests reuse what's already on disk.

The returned object is a `bathyRaster` (same class returned by
[`raster_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md)
with `depths = NULL`) and can be slotted directly into
`basemap(shapefiles = list(bathy = ...))` or passed to
[`vector_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md).

Citation requirements: EMODnet bathymetry is published under CC-BY and
must be cited if used in figures
(<https://emodnet.ec.europa.eu/en/bathymetry>). ETOPO1 also requires
citation (Amante & Eakins 2009, NOAA NGDC; see
<https://www.ncei.noaa.gov/products/etopo-global-relief-model>).

## See also

[`raster_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md),
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)

Other create shapefiles:
[`clip_shapefile()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/clip_shapefile.md),
[`geonorge_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/geonorge_bathymetry.md),
[`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md),
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md),
[`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md)

## Author

Mikko Vihtakari

## Examples

``` r
if (FALSE) { # \dontrun{
  # European waters, high resolution
  bathy <- wcs_bathymetry(c(2, 3, 54, 55), source = "emodnet")
  basemap(c(2, 3, 54, 55),
          shapefiles = list(land = dd_land, glacier = NULL,
                            bathy = bathy$raster),
          bathymetry = TRUE)

  # Global coverage (Hawaii -- outside EMODnet)
  bathy <- wcs_bathymetry(c(-160, -154, 18, 23), source = "etopo")
  basemap(c(-160, -154, 18, 23),
          shapefiles = list(land = dd_land, glacier = NULL,
                            bathy = bathy$raster),
          bathymetry = TRUE)
} # }
```
