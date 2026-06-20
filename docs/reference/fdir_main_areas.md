# Major fisheries areas (hovedomraade) of Norway

Major fisheries areas (hovedomraade) of Norway

## Usage

``` r
fdir_main_areas
```

## Format

[`sf object`](https://rdrr.io/pkg/sf/man/sf.html) containing major
fishing zones defined by the Norwegian Directorate of Fisheries.
Contains also Northwest Atlantic Fisheries Organization's divisions
where Norwegian vessels tend to fish.

## Source

[Norwegian Directorate of
Fisheries](https://open-data-fiskeridirektoratet-fiskeridir.hub.arcgis.com/)
and [Northwest Atlantic Fisheries
Organization](https://www.nafo.int/About-us/Maps)

## See also

Other datasets:
[`fdir_sub_areas`](https://mikkovihtakari.github.io/ggOceanMaps/reference/fdir_sub_areas.md),
[`ices_areas`](https://mikkovihtakari.github.io/ggOceanMaps/reference/ices_areas.md)

## Examples

``` r
if(requireNamespace("ggspatial")) {
# \donttest{ 
basemap(fdir_main_areas) + 
ggspatial::annotation_spatial(fdir_main_areas, fill = NA)
# }
}
```
