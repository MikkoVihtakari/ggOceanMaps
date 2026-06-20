# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

For AI assistants helping users *use* ggOceanMaps (not work on its source), see [AGENTS.md](AGENTS.md).

## Package overview

ggOceanMaps is an R package (v3.0.0) for plotting oceanographic data on bathymetric maps using ggplot2. It ships with low-resolution spatial data and downloads high-resolution files on demand from [ggOceanMapsLargeData](https://github.com/MikkoVihtakari/ggOceanMapsLargeData). Documentation: https://mikkovihtakari.github.io/ggOceanMaps/

## Common commands

```r
# Load package for interactive development
devtools::load_all()

# Regenerate documentation (roxygen2)
devtools::document()

# Run R CMD CHECK
devtools::check()

# Run tests only
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-basemap.R")

# Build pkgdown website
pkgdown::build_site()
```

From the terminal:
```bash
R CMD build .
R CMD check ggOceanMaps_*.tar.gz --as-cran
```

For single-article pkgdown rebuilds in this macOS environment, Pandoc may not
be on `PATH`. This worked during the latest docs pass:

```r
Sys.setenv(RSTUDIO_PANDOC = "/Applications/quarto/bin/tools/aarch64")
options(ggOceanMaps.datapath = "~/Documents/ggOceanMapsLargeData")
pkgdown::build_article("adding-graphical-elements", pkg = ".", new_process = FALSE)
```

CI runs R CMD CHECK on macOS, Windows, and Ubuntu (devel, release, oldrel-1) via `.github/workflows/R-CMD-check.yaml`.

## Architecture

### Core rendering pipeline

`basemap()` ([R/basemap.R](R/basemap.R)) is the main user-facing function. It follows this pipeline:

1. **Determine bathy style** — parses `bathy.style` string to an internal command name via `define_bathy_style()`
2. **Build data** — calls `basemap_data()` ([R/basemap_data.R](R/basemap_data.R)) which:
   - Detects the input "case" (polar limits, decimal degree limits, projected limits, sf/sp data, etc.) via `basemap_data_detect_case()`
   - Resolves which pre-made shapefile set to use via `define_shapefiles()` / `shapefile_list()`
   - Loads/downloads spatial data via `load_map_data()`
   - Clips shapefiles to the map extent via `basemap_data_crop()`
   - Computes grid line geometries via `basemap_define_grid_lines()`
3. **Assemble ggplot2 layers** — `map_cmd()` ([R/map_cmd.R](R/map_cmd.R)) returns ggplot2 command strings for each element (base, land, glaciers, bathymetry, grid definitions). These are concatenated with `+` and executed via `eval(parse(text = layers))`.
4. **Annotate output** — the returned ggplot object gets custom attributes (`bathymetry`, `glaciers`, `limits`, `polarmap`, `crs`, etc.) attached for downstream use.

`qmap()` ([R/qmap.R](R/qmap.R)) is a thin wrapper over `basemap()` for quickly plotting a data frame's coordinates.

### Projection logic

Three automatic projections selected by `define_shapefiles()` ([R/define_shapefiles.R](R/define_shapefiles.R)):
- **EPSG:3995** — Arctic Polar Stereographic (max latitude ≥ 60, min ≥ 30)
- **EPSG:3031** — Antarctic Polar Stereographic (max latitude ≤ −60, min ≤ −30)
- **EPSG:4326** — Decimal degrees (everything else)

Rotation across the antimeridian is handled by `rotate_crs()` / `dd_clip_boundary()` in [R/internal_functions.R](R/internal_functions.R).

Recent antimeridian note: explicit or rotated antimeridian/wide-span maps
should be clipped in the **projected map CRS**, not by transforming the
projected clip box back to WGS84 and intersecting there. A June 2026 fix in
`basemap_data_crop()` routes antimeridian-risk land clipping through:

- `sf::st_transform(x$shapefiles$land, crs = x$crs)`
- `clip_shapefile(..., limits = x$clip_limits, extra.validate = TRUE)`

This fixed:

- wrong land retained/cut for `basemap(limits = c(120, -120, 60, 80))`
- topology crash for `basemap(data = data.frame(lon = c(-180, -180, -160, -160), lat = c(63, 50, 50, 63)), rotate = TRUE)`

### Shapefile / data system

`shapefile_list()` ([R/shapefile_list.R](R/shapefile_list.R)) is a registry of pre-made shapefile sets: `ArcticStereographic`, `AntarcticStereographic`, `DecimalDegree`, `Svalbard`, `Europe`. Each entry specifies CRS, limits, and paths for land, glacier, and bathymetry objects.

- **Built-in low-res data** (`dd_land`, `dd_rbathy`) ship with the package in `/data/`.
- **High-res data** are downloaded from ggOceanMapsLargeData to `getOption("ggOceanMaps.datapath")` (defaults to `tempdir()`; users set a permanent path in `.Rprofile` via `.ggOceanMapsenv$datapath`).
- `load_map_data()` ([R/load_map_data.R](R/load_map_data.R)) handles existence checks, interactive download prompts, and object loading.

### Coordinate utilities

- `transform_coord()` ([R/transform_coord.R](R/transform_coord.R)) — transforms decimal degree coordinates to map projection for use in `geom_*` layers
- `guess_coordinate_columns()` ([R/guess_coordinate_columns.R](R/guess_coordinate_columns.R)) — infers which columns hold lon/lat from column names
- `auto_limits()` ([R/auto_limits.R](R/auto_limits.R)) — derives map limits from a data frame

### Bathymetry helpers (create-your-own workflow)

`raster_bathymetry()` and `vector_bathymetry()` ([R/raster_bathymetry.R](R/vector_bathymetry.R)) are standalone utilities for pre-processing external bathymetry grids (GEBCO, ETOPO, IBCAO) into the objects expected by the shapefile list.

Recent docs/vignette note: the bathymetry vignette was rewritten to describe
the five bathymetry sources more clearly and now uses pre-rendered figures for
network/download-heavy examples. A helper script for regenerating those images
lives in [dev/make_bathymetry_vignette_figs.R](dev/make_bathymetry_vignette_figs.R).

Recent overlay-vignette note: [vignettes/adding-graphical-elements.Rmd](vignettes/adding-graphical-elements.Rmd)
now contains working rendered examples for:

- Barents Sea schematic current arrows, sourced from the `Barents-Sea-currents` repo
- projected velocity fields with transformed start/end points
- projected pie charts with a second fill scale via `ggnewscale::new_scale_fill()`
- manual bathymetry bin examples
- a synthetic `raster_bathymetry()` workflow example

Recent shapefiles-vignette note: [vignettes/customising-shapefiles.Rmd](vignettes/customising-shapefiles.Rmd)
uses pre-rendered figures (same convention as the bathymetry vignette) for:

- clipping `dd_land` with `clip_shapefile()`
- `raster_bathymetry()` → `vector_bathymetry()`/`vector_land()` from a GEBCO grid
- `geonorge_bathymetry()` paired with the `"Landareal"` layer of the same `.gml`, for an Oslofjord example
- vectorising a raw EMODnet (`"wemb"`) WCS tile around Tromsø, since `wcs_bathymetry()` always discards land

The generator is [dev/make_customising_shapefiles_vignette_figs.R](dev/make_customising_shapefiles_vignette_figs.R).
It needs a local GEBCO/ETOPO/IBCAO grid and a local Geonorge "Dybdedata" `.gml`
file (e.g. for an Oslofjord municipality); both paths are read from environment
variables (`GGOCEANMAPS_USERPATH`, `GEONORGE_OSLO_GML`) rather than hardcoded,
so no personal path is committed.

Fixed bug found while building that vignette: `basemap_data.R` unconditionally
ran `sf::st_cast(x$shapefiles$land, "MULTIPOLYGON")` on the already-cropped
custom land layer (a "temporary fix to make plotly::ggplotly work"). For some
custom `vector_land()` layers at some map limits, the cropped geometry contained
a degenerate `GEOMETRYCOLLECTION` sliver, and `st_cast()` crashed with
`Error in ClosePol(unclass(x)) : polygons require at least 4 points` (confirmed
for `c(-5, 10, 50, 60)` and `c(11, 16, 67.3, 68.6)` with a custom land layer).
Fixed by extracting the polygonal parts with `sf::st_collection_extract(...,
"POLYGON")` before the cast, but only when a `GEOMETRYCOLLECTION` is actually
present (checked via `sf::st_geometry_type()`), to avoid a benign-but-noisy
"x is already of type POLYGON" warning on every normal `basemap()` call.

### Bundled datasets

Documented in [R/datasets-docs.R](R/datasets-docs.R) and [R/shapefiles-docs.R](R/shapefiles-docs.R):
- `dd_land`, `dd_rbathy` — low-res global land polygons and binned raster bathymetry
- `fdir_main_areas`, `fdir_sub_areas` — Norwegian fisheries zones
- `ices_areas` — ICES advisory areas

### Size helper functions

`LS(x)` and `FS(x)` in [R/internal_functions.R](R/internal_functions.R) convert pt values to ggplot2 line/font size units.

## Key design patterns

- **`eval(parse(text = ...))`** — `basemap()` assembles a ggplot layer string at runtime and evaluates it. When adding new `map_cmd()` entries or bathy styles, the string must evaluate in the scope of `basemap()` where `X`, `land.col`, `bathy.alpha`, etc. are defined.
- **Partial matching on shapefile names** — `match.arg()` is used throughout; "Arc", "Ar", or "Arctic" all resolve to "ArcticStereographic".
- **Options-based config** — `getOption("ggOceanMaps.datapath")`, `getOption("ggOceanMaps.bathy.style")`, `getOption("ggOceanMaps.userpath")` control runtime behavior without function arguments.
- **`@export` on internal functions** — many internal helpers are exported for advanced users despite being marked `@keywords internal`.

## Recent tests

The antimeridian/rotate regressions above are covered by smoke tests in
[tests/testthat/test-basemap-smoke.R](tests/testthat/test-basemap-smoke.R),
including:

- `basemap(limits = c(120, -120, 60, 80))`
- `basemap(data.frame(lon = c(-180, -180, -160, -160), lat = c(63, 50, 50, 63)), rotate = TRUE)`

Useful focused verification command:

```r
devtools::test(filter = "basemap-smoke")
```
