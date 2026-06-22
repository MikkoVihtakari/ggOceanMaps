---
name: Architecture
description: basemap() pipeline, projection logic, shapefile registry, eval/parse pattern
type: project
---

## Core pipeline (basemap.R → basemap_data.R → map_cmd.R)

1. `basemap()` resolves bathy style, validates args, calls `basemap_data()`
2. `basemap_data()` detects input case (polar/dec/projected/sf/sp data), resolves shapefile set, loads data, clips to extent, builds grid lines. Returns a `basemapData` list with `shapefiles`, `map.limits`, `polar.map`, `map.grid`, `proj`.
3. `basemap()` assembles a ggplot command string by concatenating `map_cmd()` snippets with `+`, then runs `eval(parse(text = layers))`. The environment where eval runs has all `basemap()` local variables in scope (`X`, `land.col`, `bathy.alpha`, etc.)
4. Returns a ggplot object with custom attributes (`ggOceanMaps` class, `crs`, `limits`, `polarmap`, etc.)

## Projection selection (define_shapefiles.R)

- max lat ≥ 60, min ≥ 30 → EPSG:3995 (ArcticStereographic)
- max lat ≤ −60, min ≤ −30 → EPSG:3031 (AntarcticStereographic)
- else → EPSG:4326 (DecimalDegree)

## Shapefile registry (shapefile_list.R)

Pre-made sets: ArcticStereographic, AntarcticStereographic, DecimalDegree, Svalbard, Europe. Each has land/glacier/bathy paths and a download URL pointing to ggOceanMapsLargeData. Names are partially matched via `match.arg()`.

## Data loading (load_map_data.R)

Checks if `.rda` files exist in `ggOceanMaps.datapath`; if not, prompts user interactively to download. Built-in data (`dd_land`, `dd_rbathy`) is always available as named R objects.

## Key internal utilities

- `rotate_crs()` / `dd_clip_boundary()` — antimeridian/rotation handling
- `define_bathy_style()` — maps abbreviated bathy style strings to internal command names
- `LS(x)` / `FS(x)` — convert pt sizes to ggplot2 units (LS: x/2.13, FS: x/2.845276)
- `quiet()` — suppresses cat() output
- `guess_coordinate_columns()` — infers lon/lat column names heuristically

## Warning: eval/parse pattern

`map_cmd()` returns raw ggplot2 R code as character strings. Adding new map elements requires writing valid R that evaluates in `basemap()`'s local scope. Bugs here produce cryptic parse/eval errors.
