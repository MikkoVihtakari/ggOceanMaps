# Rotate CRS

Rotates a CRS such that North is in the middle of two meridians

## Usage

``` r
rotate_crs(crs, meridians)
```

## Arguments

- crs:

  The CRS to be rotated in
  [`st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html)
  format

- meridians:

  A numeric vector giving the two meridians which should be used in the
  rotation

## Value

Rotated CRS in
[`st_crs`](https://r-spatial.github.io/sf/reference/st_crs.html) format

## Author

Mikko Vihtakari
