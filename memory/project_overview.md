---
name: Project overview
description: ggOceanMaps package purpose, version, dependencies, and release channels
type: project
---

ggOceanMaps v2.3.0 (development: v2.4) is a CRAN R package for plotting oceanographic/bathymetric maps using ggplot2. It supersedes the older PlotSvalbard package.

**Why:** Simplifies bathymetric map plotting globally for ocean scientists while allowing full ggplot2 customisation.

**Key dependencies:** sf, stars, ggplot2, smoothr, units. Suggests: ggspatial, cowplot, ggnewscale.

**Two data tiers:**
- Low-res data ships with the package (`dd_land`, `dd_rbathy` — ETOPO 2022, Natural Earth)
- High-res data downloaded on demand from https://github.com/MikkoVihtakari/ggOceanMapsLargeData to `getOption("ggOceanMaps.datapath")`

**Release cadence:** GitHub version updated frequently; CRAN version lags. Users are encouraged to use GitHub version for latest fixes.

**Website:** https://mikkovihtakari.github.io/ggOceanMaps/ (pkgdown)
**Bug reports:** https://github.com/MikkoVihtakari/ggOceanMaps/issues

**How to apply:** When suggesting features or fixes, consider CRAN policy constraints (no persistent writes to user directories without permission, no auto-downloads without user consent). High-res data downloads must remain interactive (user-prompted).
