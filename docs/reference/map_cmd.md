# Return map elements for basemap

An internal function to make [`basemap`](basemap.md) code more readable

## Usage

``` r
map_cmd(command, alternative = FALSE)
```

## Arguments

- command:

  basemap layer to be added

- alternative:

  logical to return alternative formmatting in certain cases. Used to
  reduce `if`-`else` statements in [`basemap`](basemap.md).

## Value

A character string containing a ggplot2 plotting command. Use
`eval(parse(text=...))` to plot the string.

## Details

This is an internal function, which is automatically run by the
[`basemap`](basemap.md) function. Common users do not need to worry
about these details. Basemap elements can added together using this
function, [`parse`](https://rdrr.io/r/base/parse.html) and
[`eval`](https://rdrr.io/r/base/eval.html).

## See also

[`basemap`](basemap.md)

## Author

Mikko Vihtakari

## Examples

``` r
## An example for utm map without glaciers or bathymetry
if (FALSE) eval(parse(text=paste(map_cmd("base"), map_cmd("land_utm"),
map_cmd("grid_utm"), map_cmd("defs_utm"), sep = "+"))) # \dontrun{}
```
