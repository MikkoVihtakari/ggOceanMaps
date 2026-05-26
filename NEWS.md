# ggOceanMaps 2.4 (development version on GitHub)

## New features

* On-demand WCS bathymetry from EMODnet via the new `wcs_bathymetry()` function
  and `bathy.style = "wcs_emodnet_blues"` (abbrev. `"wemb"`) /
  `"wcs_emodnet_grays"` (`"wemg"`). Fetches ~115 m European-waters bathymetry
  on demand; tiles cached under `getOption("ggOceanMaps.datapath")`.
* `vector_land()` extracts a land polygon from a `bathyRaster` produced by
  `raster_bathymetry()`. Pairs with `vector_bathymetry()` for build-your-own
  shapefile workflows.
* New `AGENTS.md` for AI assistants helping users *use* ggOceanMaps, and a new
  `vignette("cookbook")` of short copy-pasteable recipes.

## Testing

* Comprehensive testthat suite added: smoke tests covering the historical
  regression corpus run everywhere; vdiffr SVG snapshot tests catch
  "code runs but wrong map" regressions locally.
* Unit tests for `transform_coord()`, `auto_limits()`,
  `guess_coordinate_columns()`, `LS()`, `quiet()`, `vector_land()`,
  `wcs_bathymetry()`.

## Internal

* Performance: `basemap_data_crop()` now clips in native CRS before
  reprojecting, avoiding world-scale transforms for small map extents
  ([#55](https://github.com/MikkoVihtakari/ggOceanMaps/pull/55)).
* Performance: grid-line generation in polar maps switched from
  `lapply(unique())` to `split() + lapply()`
  ([#57](https://github.com/MikkoVihtakari/ggOceanMaps/pull/57)).
* Removed ~131 lines of dead/commented code across `basemap.R`,
  `basemap_data.R`, `raster_bathymetry.R`, `dist2land.R`
  ([#56](https://github.com/MikkoVihtakari/ggOceanMaps/pull/56),
  [#59](https://github.com/MikkoVihtakari/ggOceanMaps/pull/59),
  [#60](https://github.com/MikkoVihtakari/ggOceanMaps/pull/60),
  [#61](https://github.com/MikkoVihtakari/ggOceanMaps/pull/61),
  [#62](https://github.com/MikkoVihtakari/ggOceanMaps/pull/62)).

# ggOceanMaps 2.3.0

* Fixed anti-meridian crossing land clipping in rotated basemaps ([#53](https://github.com/MikkoVihtakari/ggOceanMaps/pull/53))
* Fixed bugs and package incompatibilities, including TopologyException ([#52](https://github.com/MikkoVihtakari/ggOceanMaps/pull/52), [#40](https://github.com/MikkoVihtakari/ggOceanMaps/issues/40))
* Fixed issue with plotting Indian and Pacific Ocean ([#51](https://github.com/MikkoVihtakari/ggOceanMaps/pull/51), [#44](https://github.com/MikkoVihtakari/ggOceanMaps/issues/44))
* Replaced `size` with `linewidth` to stop ggplot2 warning ([#49](https://github.com/MikkoVihtakari/ggOceanMaps/pull/49), [#48](https://github.com/MikkoVihtakari/ggOceanMaps/issues/48))
* Removed size-related warnings throughout the package
* Updated `grid.size`/`grid.col` behavior
* Various minor bug fixes
* Fixed a bug where both `load_map_data(shapefile_list("Arctic"))` and `shapefile_list("Arctic", get.data = TRUE)` would cause error due to changed bathymetry system.

# ggOceanMaps 2.2.0

* Added tests better explaining wrongly specified arguments
* Updated the user manual
* Fixed an issue with certain `bathy.style` abbreviations
* qmap arguments did not match those of basemap: added `bathy.alpha` and `downsample` arguments to qmap
* Fixed an [issue](https://stackoverflow.com/questions/60684049/creating-a-interactive-map-on-r-using-plotly) when trying to plot basemaps using `plotly::ggplotly()`
* Fixed [an issue with . in file path](https://github.com/MikkoVihtakari/ggOceanMaps/issues/32)
* Fixed [an issue when plotting singular points](https://github.com/MikkoVihtakari/ggOceanMaps/issues/34)
* Fixed `basemap(c(-180, 180, -90, 90))` case and turned off automatic rotation when crossing the anti-meridian. A message is shown instead.
* Turned off `expand` in `ggplot2::coord_sf()` to avoid an error when having map border at 0 meridian. 
* Fixed a case where data argument produced too wide boundaries
* [`expand.factor` should work now as designed](https://github.com/MikkoVihtakari/ggOceanMaps/issues/33)
* Fixed an error in `dist2land(binary = TRUE)`
* `get_depth()` now uses `raster_user` and returns depths as positive numeric. 
* Fixed an issue where land boundaries did not get clipped correctly when using custom crs

# ggOceanMaps 2.1.1

* Fixed a bug in bathy.style wording.
* Added [`get_depth()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/get_depth.html) function.
* Fixed a critical issue with downloads failing on Windows.
* Added detailed land shapes of Europe. Can be used by `basemap(shapefiles = "Europe")`

# ggOceanMaps 2.0.0

* Full [sf](https://r-spatial.github.io/sf/) integration. Old GIS packages for R and ggspatial dependencies removed. Since this change required rewriting of most functions, new bugs have almost certainly been introduced.
* Bathymetry system redesigned (see [this](https://mikkovihtakari.github.io/ggOceanMaps/articles/new-features.html))
* Decimal degree maps can now be plotted across the antimeridian.
* Added spatial data to ggOceanMaps making the ggOceanMapsData package not needed any longer.
* `dist2land()` now uses great circle distances on a spherical Earth ([s2](https://r-spatial.github.io/s2/)) by default and should be able to calculate distances to land anywhere around the globe.
* qmap points turned red. Addressed a long-standing issue with `shapefiles` and `shape` getting mixed. 
* Fixed a bug with shapefiles argument shortcut.
* Fixed a bug in ices_data
* Added sf support for clip_shapefile
* Added sf support for shapefiles (converts to sp, needs to be refined)
* Fixed a bug with expanded limits in decimal degree projection
* Fixed a bug where shapefile_list("all") would return multiple rows per shapefile name.

# ggOceanMaps 1.3.4

* Added shapefiles to the `x` argument shortcut in `basemap()`.
* Added limits to premade shapefiles to make visualization easier.
* Removed many CRS warning from sp and rgdal
* Added a way to control the plotting order of graticules
* Added transparency (alpha) scaling to bathymetry fill
* Added GEBCO bathymetry which is more detailed than the ArcticSterographic.
* Added EMODnet bathymetry which is more detailed than GEBCO for the Northeast Atlantic
* Improved vignette and webpage:
    * Added a way to control the plotting order of graticules.
    * New design
    * Added vignettes
* Added ICES and Norwegian directorate of fisheries areas.

# ggOceanMaps 1.2.6

* [added `x` argument to `basemap()` and `qmap()`](https://github.com/MikkoVihtakari/ggOceanMaps/issues/11)
* Conversion from PROJ4 to PROJ6. This change will make the old ggOceanMapsData files incompatible with ggOceanMaps 1.2 and vice versa. Changed most functions. 
* Added NEWS.md
* Improved premade_shapefiles and shapefile documentation.
* Started rewriting the package from `sp`, `rgeos` and `rgdal` to `sf`. 
* Moved the `rgdal` package from Imports to Suggests.
* Added `geonorge_bathymetry()`
* Added the possiblity to adjust `data` limits using the `expand.factor` parameter in `basemap()` and `qmap()`
* Improved the user manual and website.
* [Fixed an issue with other than decimal degree input rasters](https://github.com/MikkoVihtakari/ggOceanMaps/issues/2) in `raster_bathymetry()`
* [Fixed an issue with ggplot2 (>=3.3.4)](https://github.com/MikkoVihtakari/ggOceanMaps/issues/3)
         
# ggOceanMaps 1.1

* Started replacing the [PROJ4 system by the PROJ6 wkt based system](https://www.earthdatascience.org/courses/use-data-open-source-python/intro-vector-data-python/spatial-data-vector-shapefiles/epsg-proj4-coordinate-reference-system-formats-python/) by replacing "+init=epsg:NNNN" strings by "EPSG:NNNN". Did not finish the conversion. 

# ggOceanMaps 1.0.9

* First CRAN release. Contains the core of the package code rewritten from [PlotSvalbard](https://github.com/MikkoVihtakari/PlotSvalbard/)
