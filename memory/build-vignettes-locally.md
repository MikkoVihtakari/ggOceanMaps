---
name: build-vignettes-locally
description: How to render pkgdown articles/vignettes locally on this machine (pandoc path + env vars)
metadata: 
  node_type: memory
  type: reference
  originSessionId: 37a335c7-2e8d-4eac-9319-a1b36933d604
---

Rendering ggOceanMaps vignettes/pkgdown articles from `Rscript` on this machine:

- **Pandoc is not on PATH** for `Rscript`. Use RStudio's bundled copy:
  `RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"`
  (machine is arm64; set both the env var and `Sys.setenv(RSTUDIO_PANDOC=...)` inside the R call).
- Rebuild a single article: `pkgdown::build_article("bathymetry")`.
- The **ggOceanMaps.Rmd** user manual reads paths from env vars: set
  `GGOCEANMAPS_DATAPATH=$HOME/Documents/ggOceanMapsLargeData` and
  `GGOCEANMAPS_USERPATH=~/path/to/GEBCO_2025.nc`
  so high-res / user-raster examples render against real data.
- To check chunks execute without pandoc, use `knitr::knit()` instead of `rmarkdown::render()`.

See [[large-data-figure-hosting]] for where pre-rendered figures live.
