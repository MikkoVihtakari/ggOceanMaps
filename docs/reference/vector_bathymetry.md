# Create a polygon bathymetry from a raster bathymetry file

Vectorizes bathymetry rasters. Designed to be used for the output of
[`raster_bathymetry`](raster_bathymetry.md) function. Warning:
processing may take a long time if the bathymetry raster is large.

## Usage

``` r
vector_bathymetry(
  bathy,
  drop.crumbs = NULL,
  remove.holes = NULL,
  smooth = FALSE
)
```

## Arguments

- bathy:

  bathyRaster object from the
  [`raster_bathymetry`](raster_bathymetry.md) function.

- drop.crumbs:

  Single numeric value specifying a threshold (area in km2) for
  disconnected polygons which should be removed. Set to `NULL` to bypass
  the removal. Uses the
  [drop_crumbs](https://strimas.com/smoothr/reference/drop_crumbs.html)
  function.

- remove.holes:

  Single numeric value specifying a threshold (area in km2) for holes
  which should be removed. Set to `NULL` to bypass the removal. Uses the
  [fill_holes](https://strimas.com/smoothr/reference/fill_holes.html)
  function. Currently VERY slow.

- smooth:

  Logical indicating whether the pixelated contours should be smoothed.
  Uses the
  [smooth_ksmooth](https://strimas.com/smoothr/reference/smooth_ksmooth.html)
  function.

## Value

An [sf](https://r-spatial.github.io/sf/reference/st.html) object
containing the depth polygons. Uses same projection than `bathy` (see
[`CRS`](https://r-spatial.github.io/sf/reference/st_crs.html)).

## Details

The `drop.crumbs` and `remove.holes` arguments can be used to make the
resulting object smaller in file size. The `smooth` argument can be used
to remove the pixelated contours, but often increases file size. Note
also that using this option will bias the contours with respect to real
world.

## See also

Other create shapefiles: [`clip_shapefile()`](clip_shapefile.md),
[`geonorge_bathymetry()`](geonorge_bathymetry.md),
[`raster_bathymetry()`](raster_bathymetry.md)

## Author

Mikko Vihtakari
