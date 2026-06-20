# New features in ggOceanMaps version 3

``` r

library(ggOceanMaps)
```

## ggOceanMaps enters the AI age

This is a major update that brings ggOceanMaps to the AI age. From
version 3 on, the package is developed with the help of AI coding
assistants (Claude Code, Codex), which means the code is no longer
necessarily written by me — but I do still review all of it. For me this
has meant faster and, so far, more robust development. It also comes
with an increased focus on documentation, so that AI agents can better
help their users: making a map with ggOceanMaps should be a one-sentence
request, whether you type it yourself or hand it to an assistant.

The most visible part of this shift is a set of AI-focused files in the
GitHub version of the package:
[`AGENTS.md`](https://github.com/MikkoVihtakari/ggOceanMaps/blob/master/AGENTS.md),
`CLAUDE.md`, and the `memory/` folder. They give AI assistants concise,
accurate instructions for *using* ggOceanMaps — the projection rules,
the `shapefiles` contract, how
[`transform_coord()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/transform_coord.md)
works, the common pitfalls — so they spend less time guessing and more
time helping, and they help contributors’ own agents get their bearings
when developing new features. The documentation site was reorganised
around the same idea: a short [user
manual](https://mikkovihtakari.github.io/ggOceanMaps/articles/ggOceanMaps.md)
that links out to focused, in-depth articles.

So next time you ask your AI assistant for a ggOceanMaps map, point it
at <https://github.com/MikkoVihtakari/ggOceanMaps/> and ask it to
familiarise itself with the package before answering — you should get
noticeably better results.

Everything below is new or substantially improved in version 3.

## High-resolution maps of Norwegian fjords

The headline feature of v3.0.0 is something I had wanted to add for a
long time but never found the time for. Half an hour with Claude Opus
4.8 solved it, and ggOceanMaps can finally make high-resolution maps of
Norwegian fjords — a feature many of you have asked for. The new
[`wcs_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/wcs_bathymetry.md)
function downloads [EMODnet](https://emodnet.ec.europa.eu/en/bathymetry)
bathymetry (~115 m resolution) directly from their Web Coverage Service.
There are no files to manage: pick the style through `bathy.style` and
ggOceanMaps fetches, caches, and renders the right tiles for your map
extent.

Two sources are available:

- **EMODnet** (~115 m, European waters) — `bathy.style = "wemb"` (blues)
  or `"wemg"` (greys).
- **ETOPO1 from NOAA NCEI** (~1.85 km, global) — `bathy.style = "wceb"`
  or `"wceg"`. Use this when EMODnet has no coverage for your region.

Coupled with the detailed EMODnet land shapes (`shapefiles = "Europe"`),
this produces the highest-resolution maps ggOceanMaps has generated so
far. Here is Kongsfjorden in Svalbard, drawn over the detailed Svalbard
land shapes:

``` r

basemap(limits = c(10.9, 12.65, 78.83, 79.12), bathy.style = "wemb", shapefiles = "Svalbard")
```

![Kongsfjorden, Svalbard, with ~115 m EMODnet bathymetry and the
detailed Svalbard land
shapes.](https://raw.githubusercontent.com/MikkoVihtakari/ggOceanMapsLargeData/master/docs/kongsfjorden_wemb.png)

Kongsfjorden, Svalbard, with ~115 m EMODnet bathymetry and the detailed
Svalbard land shapes.

And Porsangerfjorden on the Finnmark coast of mainland Norway, with the
EMODnet `"Europe"` land shapes:

``` r

basemap(limits = c(23.9, 26.5, 69.9, 71.15), shapefiles = "Europe", bathy.style = "wemb")
```

![Porsangerfjorden, northern Norway, with ~115 m EMODnet bathymetry and
the EMODnet “Europe” land
shapes.](https://raw.githubusercontent.com/MikkoVihtakari/ggOceanMapsLargeData/master/docs/porsangerfjorden_wemb.png)

Porsangerfjorden, northern Norway, with ~115 m EMODnet bathymetry and
the EMODnet “Europe” land shapes.

The downloaded tiles are cached under
`getOption("ggOceanMaps.datapath")`, so re-rendering the exact same map
does not re-download anything. Do keep an eye on the size of that
folder, though — high-resolution tiles can make it bloat up quickly.
Bounding boxes outside a source’s coverage fail cleanly with a pointer
to the right alternative, and large areas are tiled and mosaicked
automatically. See the [Bathymetry
article](https://mikkovihtakari.github.io/ggOceanMaps/articles/bathymetry.md)
for the full list of sources and more examples.

## Build your own shapefiles

The create-your-own-bathymetry workflow is now complete.
[`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md)
turns a GEBCO/ETOPO/IBCAO grid into a `bathyRaster`, and **two**
vectorisers consume it:
[`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
for depth-contour polygons (as before) and the new
[`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md)
for a matching land polygon extracted from the same grid.

``` r

rb <- raster_bathymetry(
  bathy = "path/to/your/grid.nc",
  depths = c(50, 100, 200, 500, 1000),
  proj.out = 4326,
  boundary = c(-5, 10, 50, 60)
)

basemap(
  limits = c(-5, 10, 50, 60),
  shapefiles = list(
    land   = vector_land(rb),       # NEW in v3
    glacier = NULL,
    bathy  = vector_bathymetry(rb)
  ),
  bathymetry = TRUE
)
```

Because land and bathymetry come from the same grid, their edges line
up. The [Customising shapefiles
article](https://mikkovihtakari.github.io/ggOceanMaps/articles/customising-shapefiles.md)
walks through this pipeline,
[`clip_shapefile()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/clip_shapefile.md),
and reading Norwegian Geonorge depth data with
[`geonorge_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/geonorge_bathymetry.md).

## Expanded documentation

Version 3 splits the old single manual into a concise overview plus a
set of focused articles:

- **[Bathymetry](https://mikkovihtakari.github.io/ggOceanMaps/articles/bathymetry.md)**
  — every way to get bathymetry into a map, from the shipped
  low-resolution grid to on-demand WCS and your own rasters.
- **[Customising
  shapefiles](https://mikkovihtakari.github.io/ggOceanMaps/articles/customising-shapefiles.md)**
  — supplying your own land, glacier, and bathymetry layers.
- **[Adding graphical
  elements](https://mikkovihtakari.github.io/ggOceanMaps/articles/adding-graphical-elements.md)**
  — ocean-current arrows (velocity quivers and schematic “Figure 1”
  arrows) and pie charts on maps via `scatterpie::geom_scatterpie()`.
- **[Cookbook](https://mikkovihtakari.github.io/ggOceanMaps/articles/cookbook.md)**
  — short, copy-pasteable recipes.

## Tested and more reliable

Version 3 ships a comprehensive automated test suite: smoke tests
covering the historical regression corpus run everywhere, and `vdiffr`
SVG snapshot tests catch “code runs but wrong map” regressions. This
release also fixes several long-standing clipping problems. For example,
projected maps with decimal-degree limits used to cut off land near the
map edges — `basemap(c(-20, 30, 50, 70))` clipped off northern Norway.
The clip boundary is now densified before reprojection, so the full
extent is kept:

``` r

basemap(c(-20, 30, 50, 70), bathymetry = TRUE)
```

![](new-features-v3_files/figure-html/unnamed-chunk-5-1.png)

The map above is built entirely from the low-resolution data shipped
with the package, reprojected to Arctic stereographic on the fly — no
downloads required.

## Upgrading from version 2

Version 3 is backwards compatible with version 2 map code; existing
[`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
and
[`qmap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/qmap.md)
calls keep working. If you used the older bathymetry styles, see the
[version 2 release
notes](https://mikkovihtakari.github.io/ggOceanMaps/articles/new-features-v2.md)
and the [Bathymetry
article](https://mikkovihtakari.github.io/ggOceanMaps/articles/bathymetry.md)
for the current `bathy.style` names.
