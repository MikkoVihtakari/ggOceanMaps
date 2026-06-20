# Changelog

## ggOceanMaps 3.0.0

**Bringing ggOceanMaps to the AI age.** From this version on,
ggOceanMaps attempts to help AI agents better support their users, and
is itself developed and tested with the help of AI (Claude Code, Codex),
enabling a faster development cycle and more robust code. The package
now ships an `AGENTS.md` file and memory folder with instructions for AI
assistants, alongside a much expanded and reorganised set of articles
and recipes. This is a major release with new on-demand bathymetry
sources, build-your-own shapefile tools, a comprehensive automated test
suite, and an overhauled documentation site.

### New features

- On-demand WCS bathymetry via the new
  [`wcs_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/wcs_bathymetry.md)
  function with two sources:
  - EMODnet — `bathy.style = "wcs_emodnet_blues"` (`"wemb"`) /
    `"wcs_emodnet_grays"` (`"wemg"`). ~115 m European-waters bathymetry.
  - ETOPO1 from NOAA NCEI — `bathy.style = "wcs_etopo_blues"` (`"wceb"`)
    / `"wcs_etopo_grays"` (`"wceg"`). ~1.85 km global topo-bathy. Use
    this when EMODnet has no coverage for your region. Bounding boxes
    outside a source’s coverage error cleanly with a pointer to the
    right alternative. Large boxes are tiled and mosaicked
    automatically. Tiles cached under
    `getOption("ggOceanMaps.datapath")`.
- [`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md)
  extracts a land polygon from a `bathyRaster` produced by
  [`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md).
  Pairs with
  [`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
  for build-your-own shapefile workflows.

### Documentation and AI support

- New `AGENTS.md` with instructions for AI assistants helping users
  *use* ggOceanMaps.
- New
  [`vignette("cookbook")`](https://mikkovihtakari.github.io/ggOceanMaps/articles/cookbook.md)
  of short, copy-pasteable recipes.
- New
  [`vignette("bathymetry")`](https://mikkovihtakari.github.io/ggOceanMaps/articles/bathymetry.md)
  covering all bathymetry sources.
- New
  [`vignette("adding-graphical-elements")`](https://mikkovihtakari.github.io/ggOceanMaps/articles/adding-graphical-elements.md)
  covering ocean-current arrows (velocity quivers and schematic “Figure
  1” arrows) and pie charts on maps via `scatterpie::geom_scatterpie()`.
- New
  [`vignette("customising-shapefiles")`](https://mikkovihtakari.github.io/ggOceanMaps/articles/customising-shapefiles.md)
  covering
  [`clip_shapefile()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/clip_shapefile.md),
  the
  [`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md)
  -\>
  [`vector_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_bathymetry.md)
  /
  [`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md)
  pipeline, and
  [`geonorge_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/geonorge_bathymetry.md).
- The “Premade maps” and “Premade shapefiles” articles were rewritten to
  the current `sf`/`stars` toolchain and the current
  [`shapefile_list()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/shapefile_list.md)
  contents.
- The user manual was slimmed to concise explanations that link out to
  the in-depth articles, and the documentation site (navbar, reference
  index) was reorganised.

### Bug fixes

- Fixed land being clipped too early on projected (polar-stereographic)
  maps with decimal-degree limits, e.g. `basemap(c(-20, 30, 50, 70))`
  cut off northern Norway. The land clip boundary is now densified
  before back-projecting to WGS84.
- Fixed antimeridian / wide-span land clipping by routing the clip
  through the projected map CRS, fixing wrong land for
  `basemap(c(120, -120, 60, 80))` and a topology crash for rotated
  antimeridian data input.
- Fixed a crash (`st_cast()` on a degenerate `GEOMETRYCOLLECTION`) for
  some custom
  [`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md)
  layers at some map limits.

### Testing

- Comprehensive testthat suite added: smoke tests covering the
  historical regression corpus run everywhere; vdiffr SVG snapshot tests
  catch “code runs but wrong map” regressions locally.
- Unit tests for
  [`transform_coord()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/transform_coord.md),
  [`auto_limits()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/auto_limits.md),
  [`guess_coordinate_columns()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/guess_coordinate_columns.md),
  [`LS()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/LS.md),
  [`quiet()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/quiet.md),
  [`vector_land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/vector_land.md),
  [`wcs_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/wcs_bathymetry.md).

### Internal

- [`load_map_data()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/load_map_data.md)
  no longer calls [`menu()`](https://rdrr.io/r/utils/menu.html) in
  non-interactive sessions, so data downloads work during automated/CI
  documentation builds.
- Performance: `basemap_data_crop()` now clips in native CRS before
  reprojecting, avoiding world-scale transforms for small map extents
  ([\#55](https://github.com/MikkoVihtakari/ggOceanMaps/pull/55)).
- Performance: grid-line generation in polar maps switched from
  `lapply(unique())` to `split() + lapply()`
  ([\#57](https://github.com/MikkoVihtakari/ggOceanMaps/pull/57)).
- Removed ~131 lines of dead/commented code across `basemap.R`,
  `basemap_data.R`, `raster_bathymetry.R`, `dist2land.R`
  ([\#56](https://github.com/MikkoVihtakari/ggOceanMaps/pull/56),
  [\#59](https://github.com/MikkoVihtakari/ggOceanMaps/pull/59),
  [\#60](https://github.com/MikkoVihtakari/ggOceanMaps/pull/60),
  [\#61](https://github.com/MikkoVihtakari/ggOceanMaps/pull/61),
  [\#62](https://github.com/MikkoVihtakari/ggOceanMaps/pull/62)).

## ggOceanMaps 2.3.0

CRAN release: 2026-02-10

- Fixed anti-meridian crossing land clipping in rotated basemaps
  ([\#53](https://github.com/MikkoVihtakari/ggOceanMaps/pull/53))
- Fixed bugs and package incompatibilities, including TopologyException
  ([\#52](https://github.com/MikkoVihtakari/ggOceanMaps/pull/52),
  [\#40](https://github.com/MikkoVihtakari/ggOceanMaps/issues/40))
- Fixed issue with plotting Indian and Pacific Ocean
  ([\#51](https://github.com/MikkoVihtakari/ggOceanMaps/pull/51),
  [\#44](https://github.com/MikkoVihtakari/ggOceanMaps/issues/44))
- Replaced `size` with `linewidth` to stop ggplot2 warning
  ([\#49](https://github.com/MikkoVihtakari/ggOceanMaps/pull/49),
  [\#48](https://github.com/MikkoVihtakari/ggOceanMaps/issues/48))
- Removed size-related warnings throughout the package
- Updated `grid.size`/`grid.col` behavior
- Various minor bug fixes
- Fixed a bug where both `load_map_data(shapefile_list("Arctic"))` and
  `shapefile_list("Arctic", get.data = TRUE)` would cause error due to
  changed bathymetry system.

## ggOceanMaps 2.2.0

CRAN release: 2024-01-15

- Added tests better explaining wrongly specified arguments
- Updated the user manual
- Fixed an issue with certain `bathy.style` abbreviations
- qmap arguments did not match those of basemap: added `bathy.alpha` and
  `downsample` arguments to qmap
- Fixed an
  [issue](https://stackoverflow.com/questions/60684049/creating-a-interactive-map-on-r-using-plotly)
  when trying to plot basemaps using
  [`plotly::ggplotly()`](https://rdrr.io/pkg/plotly/man/ggplotly.html)
- Fixed [an issue with . in file
  path](https://github.com/MikkoVihtakari/ggOceanMaps/issues/32)
- Fixed [an issue when plotting singular
  points](https://github.com/MikkoVihtakari/ggOceanMaps/issues/34)
- Fixed `basemap(c(-180, 180, -90, 90))` case and turned off automatic
  rotation when crossing the anti-meridian. A message is shown instead.
- Turned off `expand` in
  [`ggplot2::coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  to avoid an error when having map border at 0 meridian.
- Fixed a case where data argument produced too wide boundaries
- [`expand.factor` should work now as
  designed](https://github.com/MikkoVihtakari/ggOceanMaps/issues/33)
- Fixed an error in `dist2land(binary = TRUE)`
- [`get_depth()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/get_depth.md)
  now uses `raster_user` and returns depths as positive numeric.
- Fixed an issue where land boundaries did not get clipped correctly
  when using custom crs

## ggOceanMaps 2.1.1

CRAN release: 2023-08-30

- Fixed a bug in bathy.style wording.
- Added
  [`get_depth()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/get_depth.html)
  function.
- Fixed a critical issue with downloads failing on Windows.
- Added detailed land shapes of Europe. Can be used by
  `basemap(shapefiles = "Europe")`

## ggOceanMaps 2.0.0

CRAN release: 2023-07-04

- Full [sf](https://r-spatial.github.io/sf/) integration. Old GIS
  packages for R and ggspatial dependencies removed. Since this change
  required rewriting of most functions, new bugs have almost certainly
  been introduced.
- Bathymetry system redesigned (see
  [this](https://mikkovihtakari.github.io/ggOceanMaps/articles/new-features-v2.html))
- Decimal degree maps can now be plotted across the antimeridian.
- Added spatial data to ggOceanMaps making the ggOceanMapsData package
  not needed any longer.
- [`dist2land()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/dist2land.md)
  now uses great circle distances on a spherical Earth
  ([s2](https://r-spatial.github.io/s2/)) by default and should be able
  to calculate distances to land anywhere around the globe.
- qmap points turned red. Addressed a long-standing issue with
  `shapefiles` and `shape` getting mixed.
- Fixed a bug with shapefiles argument shortcut.
- Fixed a bug in ices_data
- Added sf support for clip_shapefile
- Added sf support for shapefiles (converts to sp, needs to be refined)
- Fixed a bug with expanded limits in decimal degree projection
- Fixed a bug where shapefile_list(“all”) would return multiple rows per
  shapefile name.

## ggOceanMaps 1.3.4

CRAN release: 2022-09-26

- Added shapefiles to the `x` argument shortcut in
  [`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md).
- Added limits to premade shapefiles to make visualization easier.
- Removed many CRS warning from sp and rgdal
- Added a way to control the plotting order of graticules
- Added transparency (alpha) scaling to bathymetry fill
- Added GEBCO bathymetry which is more detailed than the
  ArcticSterographic.
- Added EMODnet bathymetry which is more detailed than GEBCO for the
  Northeast Atlantic
- Improved vignette and webpage:
  - Added a way to control the plotting order of graticules.
  - New design
  - Added vignettes
- Added ICES and Norwegian directorate of fisheries areas.

## ggOceanMaps 1.2.6

CRAN release: 2022-01-08

- [added `x` argument to `basemap()` and
  `qmap()`](https://github.com/MikkoVihtakari/ggOceanMaps/issues/11)
- Conversion from PROJ4 to PROJ6. This change will make the old
  ggOceanMapsData files incompatible with ggOceanMaps 1.2 and vice
  versa. Changed most functions.
- Added NEWS.md
- Improved premade_shapefiles and shapefile documentation.
- Started rewriting the package from `sp`, `rgeos` and `rgdal` to `sf`.
- Moved the `rgdal` package from Imports to Suggests.
- Added
  [`geonorge_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/geonorge_bathymetry.md)
- Added the possiblity to adjust `data` limits using the `expand.factor`
  parameter in
  [`basemap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/basemap.md)
  and
  [`qmap()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/qmap.md)
- Improved the user manual and website.
- [Fixed an issue with other than decimal degree input
  rasters](https://github.com/MikkoVihtakari/ggOceanMaps/issues/2) in
  [`raster_bathymetry()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/raster_bathymetry.md)
- [Fixed an issue with ggplot2
  (\>=3.3.4)](https://github.com/MikkoVihtakari/ggOceanMaps/issues/3)

## ggOceanMaps 1.1

CRAN release: 2021-05-21

- Started replacing the [PROJ4 system by the PROJ6 wkt based
  system](https://www.earthdatascience.org/courses/use-data-open-source-python/intro-vector-data-python/spatial-data-vector-shapefiles/epsg-proj4-coordinate-reference-system-formats-python/)
  by replacing “+init=epsg:NNNN” strings by “EPSG:NNNN”. Did not finish
  the conversion.

## ggOceanMaps 1.0.9

CRAN release: 2021-01-14

- First CRAN release. Contains the core of the package code rewritten
  from [PlotSvalbard](https://github.com/MikkoVihtakari/PlotSvalbard/)
