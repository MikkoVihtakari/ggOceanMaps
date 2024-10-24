
# ggOceanMaps

**Plot data on oceanographic maps using ggplot2. R package version
2.2.0**

<!-- badges: start -->

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4554714.svg)](https://doi.org/10.5281/zenodo.4554714)
[![R-CMD-check](https://github.com/MikkoVihtakari/ggOceanMaps/workflows/R-CMD-check/badge.svg)](https://github.com/MikkoVihtakari/ggOceanMaps/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/ggOceanMaps)](https://CRAN.R-project.org/package=ggOceanMaps)
<!-- badges: end -->

<!-- [![R-CMD-check](https://github.com/MikkoVihtakari/ggOceanMaps/workflows/R-CMD-check/badge.svg)](https://github.com/MikkoVihtakari/ggOceanMaps/actions/workflows/R-CMD-check.yaml) -->

## Overview

The ggOceanMaps package for [R](https://www.r-project.org/) allows
plotting data on bathymetric maps using
[ggplot2](https://ggplot2.tidyverse.org/reference). The package is
designed for ocean sciences and greatly simplifies bathymetric map
plotting anywhere around the globe. ggOceanMaps uses openly available
geographic data. Citing the particular data sources is advised by the
CC-BY licenses whenever maps from the package are published (see the
[*Citations and data sources*](#citations-and-data-sources) section).

The ggOceanMaps package has been developed by the [Institute of Marine
Research](https://www.hi.no/en). Note that the package comes with
absolutely no warranty and that maps generated by the package are meant
for plotting scientific data only. The maps are coarse generalizations
of third-party data and therefore inaccurate. Any [bug reports and code
fixes](https://github.com/MikkoVihtakari/ggOceanMaps/issues) are warmly
welcomed. See [*Contributions*](#contributions) for further details.

## Installation

The package is available on
[CRAN](https://CRAN.R-project.org/package=ggOceanMaps) and as a [GitHub
version](https://github.com/MikkoVihtakari/ggOceanMaps), which is
updated more frequently than the CRAN version.

Installation of the GitHub version:

``` r
remotes::install_github("MikkoVihtakari/ggOceanMaps")
```

Installation of the CRAN version:

``` r
install.packages("ggOceanMaps")
```

Note that \>2.0 versions of ggOceanMaps do not require the
ggOceanMapsData package any longer. Detailed map data are downloaded
when needed from the [ggOceanMapsLargeData](#data-path) repository.

## Usage

**ggOceanMaps** extends on
[**ggplot2**](http://ggplot2.tidyverse.org/reference/). The package uses
spatial ([**sf**](https://r-spatial.github.io/sf/)) shape- (e.g. vector)
and ([**stars**](https://r-spatial.github.io/stars/)) raster files,
[geospatial packages for
R](https://cran.r-project.org/web/views/Spatial.html) to manipulate, and
ggplot2 to plot these data. The vector and raster plotting is conducted
internally in the `basemap` function, which uses [ggplot’s sf object
plotting
capabilities](https://ggplot2.tidyverse.org/reference/ggsf.html). Maps
are plotted using the `basemap()` or `qmap()` functions that work almost
similarly to [`ggplot()` as a
base](https://ggplot2.tidyverse.org/reference/index.html) for adding
further layers to the plot using the `+` operator. The maps generated
this way already contain multiple ggplot layers. Consequently, the
[`data` argument](https://ggplot2.tidyverse.org/reference/ggplot.html)
needs to be explicitly specified inside `geom_*` functions when adding
`ggplot2` layers. Depending on the location of the map, the underlying
coordinates may be projected. Decimal degree coordinates need to be
transformed to the projected coordinates using the `transform_coord`,
[ggspatial](https://paleolimbot.github.io/ggspatial/), or [`geom_sf`
functions.](https://ggplot2.tidyverse.org/reference/ggsf.html)

``` r
library(ggOceanMaps)

dt <- data.frame(lon = c(-30, -30, 30, 30), lat = c(50, 80, 80, 50))

basemap(data = dt, bathymetry = TRUE) + 
  geom_polygon(data = transform_coord(dt), aes(x = lon, y = lat), 
               color = "red", fill = NA)
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

See the [ggOceanMaps
website](https://mikkovihtakari.github.io/ggOceanMaps/index.html),
[function
reference](https://mikkovihtakari.github.io/ggOceanMaps/reference/index.html),
and the [user
manual](https://mikkovihtakari.github.io/ggOceanMaps/articles/ggOceanMaps.html)
for how to use and modify the maps plotted by the package. You may also
find [these slides about the
package](https://aen-r-workshop.github.io/4-ggOceanMaps/ggOceanMaps_workshop.html#1)
useful.

## Data path

While ggOceanMaps allows plotting any custom-made shapefiles, the
package contains a shortcut to plot higher resolution maps for [certain
areas needed by the
author](https://mikkovihtakari.github.io/ggOceanMaps/articles/premade-maps.html)
without the need of generating the shapefiles manually. These
high-resolution shapefiles are downloaded from the
[ggOceanMapsLargeData](https://github.com/MikkoVihtakari/ggOceanMapsLargeData)
repository. As a default, the shapefiles are downloaded into a temporary
directory meaning that the user would need to download the large
shapefiles every time they restart R. This limitation is set by [CRAN
policies](https://cran.r-project.org/web/packages/policies.html). You
can define a custom folder for high-resolution shapefiles on your
computer by modifying your .Rprofile file
(e.g. `usethis::edit_r_profile()`). Add the following lines to the file:

``` r
.ggOceanMapsenv <- new.env()
.ggOceanMapsenv$datapath <- 'YourCustomPath'
```

It is smart to use a directory R has writing access to. For example
`"~/ggOceanMapsLargeData"` would work for most operating systems.

You will need to set up the data path to your .Rprofile file only once
and ggOceanMaps will find the path even though you updated your R or
packages. ggOceanMaps will inform you about your data path when you load
the package.

## Citations and data sources

The data used by the package are not the property of the Institute of
Marine Research nor the author of the package. It is, therefore,
important that you cite the data sources used in a map you generate with
the package. The spatial data used by this package have been acquired
from the following sources:

- **ggOceanMapsData land polygons.** [Natural Earth
  Data](https://www.naturalearthdata.com/downloads/10m-physical-vectors/)
  1:10m Physical Vectors with the Land and Minor Island datasets
  combined. Distributed under the [CC Public Domain
  license](https://creativecommons.org/publicdomain/) ([terms of
  use](https://www.naturalearthdata.com/about/terms-of-use/)).
- **ggOceanMapsData glacier polygons.** [Natural Earth
  Data](https://www.naturalearthdata.com/downloads/10m-physical-vectors/)
  1:10m Physical Vectors with the Glaciated Areas and Antarctic Ice
  Shelves datasets combined. Distributed under the [CC Public Domain
  license](https://creativecommons.org/publicdomain/) ([terms of
  use](https://www.naturalearthdata.com/about/terms-of-use/)).
- **ggOceanMapsData bathymetry.** [NOAA National Centers for
  Environmental Information. 2022: ETOPO 2022 15 Arc-Second Global
  Relief Model. NOAA National Centers for Environmental Information.
  DOI:
  10.25921/fd45-gt74](https://www.ncei.noaa.gov/products/etopo-global-relief-model).
  Distributed under the [U.S. Government Work
  license](https://www.usa.gov/government-works).
- **Detailed shapefiles of Svalbard and the Norwegian coast in
  [ggOceanMapsLargeData](https://github.com/MikkoVihtakari/ggOceanMapsLargeData)**
  are from [Geonorge.no](https://www.geonorge.no/). Distributed under
  the [CC BY 4.0 license](https://creativecommons.org/licenses/by/4.0/).
- **Detailed land polygons of Europe.** [European Environment
  Agency](https://www.eea.europa.eu/en/datahub/datahubitem-view/af40333f-9e94-4926-a4f0-0a787f1d2b8f).
  The coastline is a hybrid product obtained from satellite imagery from
  two projects: EUHYDRO (Pan-European hydrographic and drainage
  database) and GSHHG (A Global Self-consistent, Hierarchical,
  High-resolution Geography Database), as well as some manual amendments
  to meet requirements from EU Nature Directives, Water Framework
  Directive and Marine Strategy Framework. Distributed under the [CC BY
  4.0 license](https://creativecommons.org/licenses/by/4.0/) ([terms of
  use](https://www.eea.europa.eu/en/legal-notice))
  <!-- - **Detailed bathymetry of the Arctic (IBCAO), Northern Hemisphere (GEBCO) and the Barents Sea (BarentsSea) in [ggOceanMapsLargeData](https://github.com/MikkoVihtakari/ggOceanMapsLargeData)** are vectorized from the [General Bathymetric Chart of the Oceans](https://www.gebco.net/data_and_products/gridded_bathymetry_data/) 15-arcsecond 2023 grid. [Terms of use](https://www.gebco.net/data_and_products/gridded_bathymetry_data/gebco_2019/grid_terms_of_use.html) -->
  <!-- - **Detailed bathymetry of the Northeast Atlantic (EMODned) in [ggOceanMapsLargeData](https://github.com/MikkoVihtakari/ggOceanMapsLargeData)** is vectorized from the [European Marine Observation and Data Network](https://www.emodnet-bathymetry.eu/data-products) 3.75-arcsecond grid. [Terms of use](https://www.emodnet-bathymetry.eu/home/terms-of-use) -->

Further, please cite the package whenever maps generated by the package
are published. For up-to-date citation information, please use:

``` r
citation("ggOceanMaps")
#> To cite package 'ggOceanMaps' in publications use:
#> 
#>   Vihtakari M (2024). _ggOceanMaps: Plot Data on Oceanographic Maps
#>   using 'ggplot2'_. R package version 2.2.0,
#>   <https://CRAN.R-project.org/package=ggOceanMaps>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ggOceanMaps: Plot Data on Oceanographic Maps using 'ggplot2'},
#>     author = {Mikko Vihtakari},
#>     year = {2024},
#>     note = {R package version 2.2.0},
#>     url = {https://CRAN.R-project.org/package=ggOceanMaps},
#>   }
```

## Getting help

If your problem does not involve bugs in ggOceanMaps, the quickest way
of getting help can be posting your problem to [Stack
Overflow](https://stackoverflow.com/search?q=ggoceanmaps).
Alternatively, you are welcome to use the [issues
section](https://github.com/MikkoVihtakari/ggOceanMaps/issues) on
GitHub. Please remember to include a reproducible example that
illustrates your problem and to add links to potential cross-posts.

## Contributions

Any contributions to the package are more than welcome. Please contact
the package maintainer Mikko Vihtakari (<mikko.vihtakari@hi.no>) to
discuss your ideas on improving the package. Bug reports and corrections
should be submitted directly to [the GitHub
site](https://github.com/MikkoVihtakari/ggOceanMaps/issues). Please
include a [minimal reproducible
example](https://en.wikipedia.org/wiki/Minimal_working_example).
Considerable contributions to the package development will be credited
with authorship.
