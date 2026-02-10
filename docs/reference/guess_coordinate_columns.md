# Guess longitude and latitude columns in a data frame

An internal function to make [`basemap`](basemap.md) code more readable

## Usage

``` r
guess_coordinate_columns(data, lon = NULL, lat = NULL)
```

## Arguments

- data:

  Dataframe containing data for which the limits should be calculated.

- lon, lat:

  Character defining the name of the longitude and latitude columns in
  `data`. Use `NULL` to guess the longitude and/or latitude columns in
  `x`.

## Value

A named vector of colummn names.

## Details

This is an internal function, which is automatically run by the
[`basemap`](basemap.md) function.

## See also

[`auto_limits`](auto_limits.md),
[`transform_coord`](transform_coord.md), [`basemap`](basemap.md)

## Author

Mikko Vihtakari
