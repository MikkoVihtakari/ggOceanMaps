# ggOceanMaps 2.2 (development version on GitHub)

# ggOceanMaps 2.2.0

* Add tests better explaining wrongly specified arguments
* Update the user manual
* Fix an issue with certain `bathy.style` abbreviations
* qmap arguments did not match those of basemap: add `bathy.alpha` and `downsample` arguments to qmap
* Fix an [issue](https://stackoverflow.com/questions/60684049/creating-a-interactive-map-on-r-using-plotly) when trying to plot basemaps using `plotly::ggplotly()`
* Fix [an issue with . in file path](https://github.com/MikkoVihtakari/ggOceanMaps/issues/32)
* Fix [an issue when plotting singular points](https://github.com/MikkoVihtakari/ggOceanMaps/issues/34)
* Fix `basemap(c(-180, 180, -90, 90))` case and turn off automatic rotation when crossing the anti-meridian. A message is shown instead.
* Turn off `expand` in `ggplot2::coord_sf()` to avoid an error when having map border at 0 meridian. 
* Fix a case where data argument produced too wide boundaries
* [`expand.factor` should work now as designed](https://github.com/MikkoVihtakari/ggOceanMaps/issues/33)
* Fix an error in `dist2land(binary = TRUE)`
* `get_depth()` now uses `raster_user` and returns depths as positive numeric. 
* Fix an issue where land boundaries did not get clipped correctly when using custom crs

# ggOceanMaps 2.1.1

* Fix a bug in bathy.style wording.
* Add [`get_depth()`](https://mikkovihtakari.github.io/ggOceanMaps/reference/get_depth.html) function.
* Fix a critical issue with downloads failing on Windows.
* Add detailed land shapes of Europe. Can be used by `basemap(shapefiles = "Europe")`

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
