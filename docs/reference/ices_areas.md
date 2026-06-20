# ICES Advisory Areas

ICES Advisory Areas

## Usage

``` r
ices_areas
```

## Format

[`sf object`](https://rdrr.io/pkg/sf/man/sf.html) containing ICES
Advisory Areas.

## Source

[International Council for the Exploration of the
Sea](https://www.ices.dk/)

## See also

Other datasets:
[`fdir_main_areas`](https://mikkovihtakari.github.io/ggOceanMaps/reference/fdir_main_areas.md),
[`fdir_sub_areas`](https://mikkovihtakari.github.io/ggOceanMaps/reference/fdir_sub_areas.md)

## Examples

``` r
if(requireNamespace("ggspatial")) {
# \donttest{
basemap(ices_areas) + 
ggspatial::annotation_spatial(ices_areas, fill = NA)
# }
}
```
