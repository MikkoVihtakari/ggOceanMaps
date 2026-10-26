# Raster bathymetry layer for basemap

An internal replacement for
[`geom_stars`](https://r-spatial.github.io/stars/reference/geom_stars.html)
used to plot raster bathymetries in
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md).

## Usage

``` r
bathy_raster_layer(x, downsample = 0, show.legend = TRUE, alpha = 1)
```

## Arguments

- x:

  A `stars` object containing the bathymetry raster. See
  [`st_as_stars`](https://r-spatial.github.io/stars/reference/st_as_stars.html).

- downsample:

  Integer passed to
  [`st_downsample`](https://r-spatial.github.io/stars/reference/st_downsample.html).
  0 (default) does not downsample.

- show.legend:

  Logical indicating whether the fill legend should be shown.

- alpha:

  Transparency of the raster (0-1).

## Value

A ggplot2 layer.

## Details

This is an internal function which is automatically run by
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md).
Common users do not need to worry about these details.

[`stars::geom_stars()`](https://r-spatial.github.io/stars/reference/geom_stars.html)
turns the entire raster into a tibble and, when called with
`na.action = na.omit`, keeps a named index of every dropped cell in the
`"na.action"` attribute. For the million-cell rasters
[`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
routinely produces, that attribute is larger than the plotted data
itself. This function instead extracts only the cells that contain data,
and hands them to `GeomBathyRaster`, a
[`GeomRaster`](https://ggplot2.tidyverse.org/reference/Geom.html)
variant that neither materialises the `xmin`/`xmax`/`ymin`/`ymax`
columns (four doubles per cell, needed only for the extent of the raster
as a whole) nor warns about uneven pixel intervals when the raster
contains entirely empty columns or rows (issue \#69). Rasters that are
not regular grids are passed on to
[`stars::geom_stars()`](https://r-spatial.github.io/stars/reference/geom_stars.html).

## See also

[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)

## Author

Mikko Vihtakari
