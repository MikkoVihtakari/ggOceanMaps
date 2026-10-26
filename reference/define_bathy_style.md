# Define bathymetry style for [`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)

Defines bathymetry style to be used in
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)

## Usage

``` r
define_bathy_style(x)
```

## Arguments

- x:

  Character argument giving the input bathymetry style. Partially
  matched and can be abbreviated. See `bathy.style` in
  [`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md).

## Value

Returns a named character vector with the name pointing to the
bathymetry style and value to internal map element command for
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
(see
[`map_cmd`](https://mikkovihtakari.github.io/ggOceanMaps/reference/map_cmd.md)).

## Author

Mikko Vihtakari
