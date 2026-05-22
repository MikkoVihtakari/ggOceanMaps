# AGENTS.md — using ggOceanMaps with an AI assistant

This file is written for AI assistants that are helping a user *use* ggOceanMaps
to plot oceanographic data. For working on the package source itself, see
[CLAUDE.md](CLAUDE.md).

## What this package does

ggOceanMaps plots data on bathymetric maps using ggplot2. It ships with
low-resolution global land + bathymetry data and downloads high-resolution
files on demand. The main entry point is `basemap()`; everything else is a
ggplot2 layer added with `+`.

```r
library(ggOceanMaps)
basemap(limits = c(-20, 30, 50, 70))                     # Europe
basemap(60)                                              # polar Arctic
basemap(60, bathymetry = TRUE)                           # polar Arctic with bathy
basemap(data = data.frame(lon = c(-10, 20), lat = c(55, 65)))  # fit to data
```

## Minimal working examples

### Plot a region by lon/lat box

```r
basemap(limits = c(xmin, xmax, ymin, ymax))
# e.g.
basemap(limits = c(-20, 30, 50, 70))
```

### Plot a polar map

A single number means "show down to this latitude, polar projection":

```r
basemap(60)        # Arctic, shows 60°N to the pole
basemap(-60)       # Antarctic, shows 60°S to the pole
```

### Plot from a data frame

`basemap()` fits limits automatically and picks a projection:

```r
basemap(data = my_df)               # auto-fits limits
qmap(my_df)                         # one-liner: plot the points on a basemap
qmap(my_df, color = I("red"))
```

`my_df` needs columns the package can recognize as longitude and latitude:
`lon`, `long`, `longitude`, `x` (and the equivalents for lat). Other names
work too via `guess_coordinate_columns()`.

### Add your own ggplot2 layers

`basemap()` returns a regular ggplot object. **If the map is projected (polar or
custom CRS), decimal-degree data must be transformed first** — otherwise points
land in the wrong place:

```r
# Use transform_coord() to project decimal-degree data
dt <- transform_coord(my_df, bind = TRUE)   # adds lon.proj / lat.proj
basemap(data = my_df) +
  geom_point(data = dt, aes(x = lon.proj, y = lat.proj))

# Or use ggspatial::geom_spatial_*, which handles the transform internally
basemap(limits = c(-20, 30, 50, 70)) +
  ggspatial::geom_spatial_point(data = my_df, aes(x = lon, y = lat),
                                crs = 4326)
```

For unprojected (decimal-degree) maps you can use `geom_point()` directly with
`x = lon, y = lat`.

### Add bathymetry

Built-in (no download):

```r
basemap(limits = c(-20, 30, 50, 70), bathymetry = TRUE)
```

Higher-resolution bathymetry needs the
[ggOceanMapsLargeData](https://github.com/MikkoVihtakari/ggOceanMapsLargeData)
download. Set a permanent download path once in `.Rprofile`:

```r
options(ggOceanMaps.datapath = "~/path/to/ggOceanMapsLargeData")
```

Then:

```r
basemap(limits = c(-20, 30, 50, 70), bathy.style = "rcb")  # continuous blues
```

User-supplied raster (any GEBCO / ETOPO / IBCAO NetCDF):

```r
options(ggOceanMaps.userpath = "~/Downloads/GEBCO_2025.nc")
basemap(limits = c(-20, 30, 50, 70), bathy.style = "rub")
```

`bathy.style` strings are `type_resolution_color`:

| code | type | resolution | colour |
|---|---|---|---|
| `rbb` | raster | binned | blues (default) |
| `rcb` | raster | continuous | blues (needs largedata) |
| `rub` | raster | user | blues (needs userpath) |
| `pb` | poly | binned | blues (needs largedata) |
| `cb` | contour | binned | blues (needs largedata) |
| ...replace `b` with `g` for grays | | | |

### Use a custom CRS

```r
basemap(limits = c(0, 15, 55, 65), crs = 32631)   # UTM zone 31N
```

### Save the figure

It's a ggplot, so `ggsave()` works:

```r
p <- basemap(60, bathymetry = TRUE)
ggsave("arctic.png", p, width = 6, height = 6, dpi = 300)
```

## Common pitfalls

1. **`limits` order is `c(xmin, xmax, ymin, ymax)`**, not min/max pairs. So
   `c(-20, 30, 50, 70)` is "lon −20 to 30, lat 50 to 70". A single number means
   polar projection bounded at that latitude.

2. **Projected maps need `transform_coord()` for added data.** Polar maps and
   custom-CRS maps are not in decimal degrees. Using `geom_point(aes(x = lon,
   y = lat))` on a polar basemap places points at the wrong location.
   `transform_coord(data, bind = TRUE)` adds `lon.proj` and `lat.proj` columns;
   plot those. Or use `ggspatial::geom_spatial_point()`.

3. **Antimeridian crossing**: limits like `c(160, -160, 60, 80)` work, but use
   `rotate = TRUE` to get a sensible-looking map. Without `rotate`, the
   package prints a message and shows the whole world.

4. **Bathymetry styles need data.** `bathy.style = "rcb"`, `"pb"`, `"cb"`
   require the ggOceanMapsLargeData download (`options(ggOceanMaps.datapath)`).
   `"rub"` / `"rug"` require `options(ggOceanMaps.userpath = "path/to/file.nc")`.
   The default `bathymetry = TRUE` uses the bundled binned raster and works
   without any download.

5. **Don't add `+ coord_sf()` manually.** `basemap()` already sets up
   `coord_sf()` with the right projection and limits. Adding another will
   reset what the package did and usually clip the map incorrectly.

6. **`shapefiles` vs `shape`**: `shapefiles` (plural) selects the underlying
   spatial dataset (`"Arctic"`, `"Europe"`, …). `shape` is a ggplot2 aesthetic
   for marker shape. Easy to confuse.

7. **First call may take a while.** If `bathymetry = TRUE` triggers a download,
   the first basemap is slow. Subsequent calls reuse the cached data.

## Where to find more

- User manual: `vignette("ggOceanMaps")`
- Cookbook of recipes: `vignette("cookbook")`
- Function reference: <https://mikkovihtakari.github.io/ggOceanMaps/reference/>
- Issues / bug reports: <https://github.com/MikkoVihtakari/ggOceanMaps/issues>
