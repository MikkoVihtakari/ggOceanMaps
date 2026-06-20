# Norwegian sub-areas (lokasjon) for commercial fishing

Norwegian sub-areas (lokasjon) for commercial fishing

## Usage

``` r
fdir_sub_areas
```

## Format

[`sf object`](https://r-spatial.github.io/sf/reference/sf.html)
containing major fishing zones defined by the Norwegian Directorate of
Fisheries.

## Source

[Norwegian Directorate of
Fisheries](https://open-data-fiskeridirektoratet-fiskeridir.hub.arcgis.com/)

## See also

Other datasets:
[`fdir_main_areas`](https://mikkovihtakari.github.io/ggOceanMaps/reference/fdir_main_areas.md),
[`ices_areas`](https://mikkovihtakari.github.io/ggOceanMaps/reference/ices_areas.md)

## Examples

``` r
if(requireNamespace("ggspatial")) {
# \donttest{
basemap(fdir_sub_areas) + 
ggspatial::annotation_spatial(fdir_sub_areas, fill = NA)
# }
}
```
