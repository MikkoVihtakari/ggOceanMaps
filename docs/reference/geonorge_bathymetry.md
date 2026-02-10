# Open Geonorge bathymetry shapefiles

Opens and formats Geonorge bathymetry shapefiles ready for plotting in
ggOceanMaps

## Usage

``` r
geonorge_bathymetry(filepath, layer = NULL, verbose = FALSE)
```

## Arguments

- filepath:

  Character string defining the path to the `.gml` file. Must contain
  the file extension.

- layer:

  Character string defining the layer containing depth information. If
  `NULL` assumed to be "dybdeareal".

- verbose:

  Logical indicating whether information the reading process should be
  returned.

## Value

An [sf](https://r-spatial.github.io/sf/reference/st.html) object
containing the depth polygons. Uses same projection than `bathy` (see
[`CRS`](https://r-spatial.github.io/sf/reference/st_crs.html)).

## Details

You can download the bathymetry polygon shapefiles from
[Geonorge](https://kartkatalog.geonorge.no/metadata/kartverket/dybdedata/2751aacf-5472-4850-a208-3532a51c529a).
Download the file in `GLM` format.

## See also

Other create shapefiles: [`clip_shapefile()`](clip_shapefile.md),
[`raster_bathymetry()`](raster_bathymetry.md),
[`vector_bathymetry()`](vector_bathymetry.md)

## Author

Mikko Vihtakari
