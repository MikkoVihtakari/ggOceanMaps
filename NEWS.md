# ggOceanMaps 1.3 (development version)

* Removed many CRS warning from sp and rgdal
* Added a way to control the plotting order of graticules
* Add transparency (alpha) scaling to bathymetry fill
* Many bug fixes
* Add IBCAO bathymetry which is more detailed than the ArticSterographic
* Improve vignette and webpage. Add a way to control the plotting order of graticules.

# ggOceanMaps 1.2

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

* Generally bug fixes. Started replacing the [PROJ4 system by the PROJ6 wkt based system](https://www.earthdatascience.org/courses/use-data-open-source-python/intro-vector-data-python/spatial-data-vector-shapefiles/epsg-proj4-coordinate-reference-system-formats-python/) by replacing "+init=epsg:NNNN" strings by "EPSG:NNNN". Did not finish the conversion. 

# ggOceanMaps 1.0.9

* First CRAN release. Contains the core of the package code rewritten from [PlotSvalbard](https://github.com/MikkoVihtakari/PlotSvalbard/)
