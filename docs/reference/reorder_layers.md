# Move `basemap` land, glacier and grid layers on top of other ggplot layers

Moves existing land, glacier and grid layers on top of other layers.
Useful for hiding region polygons under land.

## Usage

``` r
reorder_layers(p)
```

## Arguments

- p:

  ggplot object from the [`basemap`](basemap.md) function.

## Value

Returns a ggplot object with land, glacier and grid layers on top.

## Details

This function has not been tested properly yet and is likely to contain
bugs.

## See also

Other customize shapefiles: [`auto_limits()`](auto_limits.md),
[`theme_map()`](theme_map.md)

## Author

Mikko Vihtakari

## Examples

``` r
if(requireNamespace("ggspatial", quietly = TRUE)) {
# \donttest{
 data("ices_areas")
 p <- basemap(c(-20, 15, 50, 70)) + 
   ggspatial::annotation_spatial(ices_areas, aes(fill = Area_Full), show.legend = FALSE)
 
 # Polygons on top of land
 p
 
 # Move land on top
 reorder_layers(p)
 # }
 }
```
