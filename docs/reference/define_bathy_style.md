# Define bathymetry style for [`basemap`](basemap.md)

Defines bathymetry style to be used in [`basemap`](basemap.md)

## Usage

``` r
define_bathy_style(x)
```

## Arguments

- x:

  Character argument giving the input bathymetry style. Partially
  matched and can be abbreviated. See `bathy.style` in
  [`basemap`](basemap.md).

## Value

Returns a named character vector with the name pointing to the
bathymetry style and value to internal map element command for
[`basemap`](basemap.md) (see [`map_cmd`](map_cmd.md)).

## Author

Mikko Vihtakari
