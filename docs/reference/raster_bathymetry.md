# Simplify a bathymetry raster ready for vectorization

Simplifies bathymetry raster ready for the
[`vector_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
function. Warning: processing may take a long time if the bathymetry
raster is large.

## Usage

``` r
raster_bathymetry(
  bathy,
  depths,
  proj.out = NULL,
  proj.bathy = NULL,
  boundary = NULL,
  warp = FALSE,
  estimate.land = FALSE,
  downsample = NULL,
  verbose = TRUE
)
```

## Arguments

- bathy:

  A [stars](https://r-spatial.github.io/stars/reference/read_stars.html)
  object or a string giving the path to a bathymetry NetCDF or grd file

- depths:

  Numeric vector giving the cut points for depth contours (see
  [`cut`](https://rdrr.io/r/base/cut.html)). If `NULL`, no depth
  aggregation will be made. This option is suitable for raster
  bathymetries passed directly to `basemap`.

- proj.out:

  A character string specifying the [coordinate reference
  system](https://r-spatial.github.io/sf/reference/st_crs.html) (CRS)
  argument for the output. See
  [`st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html) and
  [proj.org](https://proj.org/). If `NULL`, the projection is retrieved
  from `bathy` and the output will not be reprojected saving processing
  time (since `proj.out` and `proj.bathy` would match.

- proj.bathy:

  A character string specifying the
  [`CRS`](https://r-spatial.github.io/sf/reference/st_crs.html) for the
  input (`bathy`). Only required if `bathy` lacks CRS information. If
  `NULL`, `"EPSG:4326"` is assumed.

- boundary:

  A [st_polygon](https://r-spatial.github.io/sf/reference/st.html)
  object, text string defining the file path to a spatial polygon,
  [bounding box](https://r-spatial.github.io/sf/reference/st_bbox.html),
  or a numeric vector of length 4 giving the boundaries for which
  `bathy` should be cut to. Should be given as **decimal degrees**. If
  unnamed numeric vector, the first element defines the minimum
  longitude, the second element the maximum longitude, the third element
  the minimum latitude and the fourth element the maximum latitude of
  the bounding box. You can also use the sf bounding box format as named
  vector. Use `NULL` not to cut `bathy`.

- warp:

  Logical indicating whether the resulting grid should be resampled to a
  new CRS if `proj.out != proj.bathy` using the
  [`st_warp`](https://r-spatial.github.io/stars/reference/st_warp.html)
  function. A time-consuming operation, but necessary when CRS changes
  in raster bathymetries. Not required if the next step is to vectorise
  the bathymetry.

- estimate.land:

  Logical indicating whether to include land to the output. Can be used
  in the following
  [`vector_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
  step to estimate land polygons.

- downsample:

  An integer defining how many rows in `bathy` should be skipped to
  reduce the size (and resolution). 1 skips every second row, 2 every
  second and third. See
  [`st_downsample`](https://r-spatial.github.io/stars/reference/st_downsample.html).
  Set to `NULL` (default) to skip downsampling.

- verbose:

  Logical indicating whether information about progress and guessed
  projection should be returned. Set to `FALSE` to make the function
  silent.

## Value

A list with a
[stars](https://r-spatial.github.io/stars/reference/read_stars.html)
object the containing projected bathymetry defined by the `proj.out`
argument and a data frame of depth intervals.

## Details

You can use
[GEBCO](https://www.gebco.net/data-products/gridded-bathymetry-data),
[IBCAO](https://www.gebco.net/data-products/gridded-bathymetry-data/arctic-ocean),
[ETOPO](https://www.ncei.noaa.gov/products/etopo-global-relief-model)
bathymetry grids downloaded from respective sources as the `bathy`
argument. The bathymetry grids read from files must be in any format
read by
[`read_stars`](https://r-spatial.github.io/stars/reference/read_stars.html).
Alternatively use the
[`marmap::getNOAA.bathy`](https://rdrr.io/pkg/marmap/man/getNOAA.bathy.html)
function to download ETOPO1 bathymetry and convert it to a raster object
using the
[`marmap::as.raster`](https://rdrr.io/pkg/marmap/man/as.raster.html)
function.

Note that the size of the output is heavily influenced by the number of
depth contours (`depths`) as well as the resolution of `bathy` and
choice of `downsample`. To make the
[`vector_bathymetry`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
function and consequent plotting faster, limiting the details of the
bathymetry raster may be desirable.

## References

GEBCO Compilation Group (2025) GEBCO 2025
(doi:10.5285/37c52e96-24ea-67ce-e063-7086abc05f29). URL:
<https://www.gebco.net/data-products-gridded-bathymetry-data/gebco2025-grid>.
NOAA National Centers for Environmental Information. 2022: ETOPO 2022 15
Arc-Second Global Relief Model. NOAA National Centers for Environmental
Information.
[doi:10.25921/fd45-gt74](https://doi.org/10.25921/fd45-gt74) .

## See also

Other create shapefiles:
[`clip_shapefile()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/clip_shapefile.md),
[`geonorge_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/geonorge_bathymetry.md),
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md),
[`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md),
[`wcs_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/wcs_bathymetry.md)

## Author

Mikko Vihtakari
