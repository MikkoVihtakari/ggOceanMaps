# Create a land polygon from a raster bathymetry

Extracts the land area from a `bathyRaster` object produced by
[`raster_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md)
and returns it as an [sf](https://rdrr.io/pkg/sf/man/st.html) polygon
layer suitable for use in the `shapefiles` argument of
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md).
Warning: processing may take a long time if the bathymetry raster is
large.

## Usage

``` r
vector_land(bathy, drop.crumbs = NULL, remove.holes = NULL, smooth = FALSE)
```

## Arguments

- bathy:

  A `bathyRaster` object from
  [`raster_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md).
  Land cells must be detectable either as `NA` (the default when
  `depths = NULL` or `estimate.land = FALSE`) or as the factor level
  `"land"` (when `estimate.land = TRUE`).

- drop.crumbs:

  Single numeric value specifying a threshold (area in km2) for
  disconnected polygons which should be removed. Set to `NULL` to bypass
  the removal. Uses the
  [drop_crumbs](https://rdrr.io/pkg/smoothr/man/drop_crumbs.html)
  function.

- remove.holes:

  Single numeric value specifying a threshold (area in km2) for holes
  which should be removed. Set to `NULL` to bypass the removal. Uses the
  [fill_holes](https://rdrr.io/pkg/smoothr/man/fill_holes.html)
  function. Currently VERY slow.

- smooth:

  Logical indicating whether the pixelated contours should be smoothed.
  Uses the
  [smooth_ksmooth](https://rdrr.io/pkg/smoothr/man/smooth_ksmooth.html)
  function.

## Value

An [sf](https://rdrr.io/pkg/sf/man/st.html) object containing the land
polygons in the same projection as `bathy$raster`.

## Details

The `drop.crumbs` and `remove.holes` arguments can be used to reduce the
resulting file size. The `smooth` argument removes the pixelated
contours but increases file size and biases the polygon with respect to
the underlying raster.

Use `vector_land()` together with
[`vector_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
to build a matched land + bathymetry pair from a single source raster
(e.g. GEBCO, ETOPO, IBCAO) — see the example.

## See also

[`raster_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md),
[`vector_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)

Other create shapefiles:
[`clip_shapefile()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/clip_shapefile.md),
[`geonorge_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/geonorge_bathymetry.md),
[`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md),
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md),
[`wcs_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/wcs_bathymetry.md)

## Author

Mikko Vihtakari
