# Plot Data on Oceanographic Maps using ggplot2

Uses ggplot2 syntax and shape files to plot research data on
oceanographic maps anywhere around the globe.

## Details

The general map-making function for ggOceanMaps is
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md).
This function creates a "canvas" on which research data can be plotted.
The
[`basemap`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
function is analogous to
[`ggplot`](https://rdrr.io/pkg/ggplot2/man/ggplot.html) function from
the ggplot2 package. Remember to use `data = <<NAMEOFDATASET>>` for
additional geometries you plot on `basemaps`
(`basemap(60) + geom_point(data = data.frame(lon = 50, lat = 70), aes(x = lon, y = lat))`
as an example). As a shortcut, you may also use qmap(data.frame(lon =
c(10, 50), lat = c(60, 70))). Bathymetry is plotted using the
`bathymetry` argument.

## See also

Useful links:

- <https://mikkovihtakari.github.io/ggOceanMaps/>

- Report bugs at <https://github.com/MikkoVihtakari/ggOceanMaps/issues>

## Author

**Maintainer**: Mikko Vihtakari <mikko.vihtakari@hi.no>
([ORCID](https://orcid.org/0000-0003-0371-4319)) (affiliation: Institute
of Marine Research)

Authors:

- Mikko Vihtakari <mikko.vihtakari@hi.no>
  ([ORCID](https://orcid.org/0000-0003-0371-4319)) (affiliation:
  Institute of Marine Research)

Other contributors:

- Roger Bivand \[contributor\]

- Hadley Wickham \[contributor\]
