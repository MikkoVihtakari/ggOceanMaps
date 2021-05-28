# ggOceanMaps (development version)

* Added NEWS.md
* Improved premade_shapefiles and shapefile documentation.
* Continued the PROJ4 to PROJ6 conversion. This change will eventually make the old ggOceanMapsData files incompatible with ggOceanMaps 1.2 and vice versa. 
         
# ggOceanMaps 1.1

* Generally bug fixes. Started replacing the [PROJ4 system by the PROJ6 wkt based system](https://www.earthdatascience.org/courses/use-data-open-source-python/intro-vector-data-python/spatial-data-vector-shapefiles/epsg-proj4-coordinate-reference-system-formats-python/) by replacing "+init=epsg:NNNN" strings by "EPSG:NNNN". Did not finish the conversion. 

# ggOceanMaps 1.0

* First CRAN release. Contains the core of the package code rewritten from [PlotSvalbard](https://github.com/MikkoVihtakari/PlotSvalbard/)
