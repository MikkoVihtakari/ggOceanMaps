# Generates the pre-rendered example figures used in
# vignettes/customising-shapefiles.Rmd. Not run during vignette compilation.
# Re-run manually when the examples change, then push ggOceanMapsLargeData.
#
#   Rscript dev/make_customising_shapefiles_vignette_figs.R
#
# Two examples need a local file that is not part of this repo. Set these as
# environment variables (e.g. in ~/.Renviron) rather than hardcoding a path:
#   GGOCEANMAPS_USERPATH - a GEBCO/ETOPO/IBCAO grid (raster_bathymetry example)
#   GEONORGE_OSLO_GML     - an unzipped Geonorge "Dybdedata" .gml file for Oslo
#                            municipality, from
#                            https://kartkatalog.geonorge.no/metadata/kartverket/dybdedata/2751aacf-5472-4850-a208-3532a51c529a

options(
  ggOceanMaps.datapath = "~/Documents/ggOceanMapsLargeData",
  ggOceanMaps.userpath = Sys.getenv("GGOCEANMAPS_USERPATH")
)

devtools::load_all(".")
library(ggplot2)
library(sf)

outdir <- "/Users/a22357/ownCloud/Workstuff/R/Github/ggOceanMapsLargeData/docs"

save_fig <- function(p, name, w = 6, h = 5, dpi = 150) {
  ggsave(file.path(outdir, name), plot = p, width = w, height = h,
         dpi = dpi, bg = "white")
  message("saved ", name)
}

# 1. Clipping an existing layer ---------------------------------------------
land <- clip_shapefile(dd_land, limits = c(-5, 10, 50, 60))
save_fig(
  basemap(limits = c(-5, 10, 50, 60),
          shapefiles = list(land = land, glacier = NULL, bathy = NULL)),
  "shapes_clip.png"
)

# 2. raster_bathymetry() -> vector_bathymetry()/vector_land() ---------------
# Same North Sea domain as the clipping example above, with depth breaks
# that suit its mostly-shallow shelf and the Norwegian Trench.
#
# drop.crumbs is left NULL here: the southern North Sea coast (Belgium,
# the Netherlands) carries many sandbanks and islets close to the area
# threshold, and vector_bathymetry()/vector_land() each apply drop.crumbs
# independently. A shared non-NULL threshold drops a small island from one
# layer without necessarily dropping the matching sliver of shallow water
# from the other, leaving tiny gaps (neither land nor bathymetry) right at
# the coast.
north_sea <- c(-5, 10, 50, 60)
rb <- raster_bathymetry(
  getOption("ggOceanMaps.userpath"),
  depths = c(20, 50, 100, 200, 500),
  boundary = north_sea,
  estimate.land = TRUE,
  verbose = FALSE
)
vb <- vector_bathymetry(rb, drop.crumbs = NULL)
vl <- vector_land(rb, drop.crumbs = NULL)

# GEBCO/ETOPO NetCDF grids are read with a non-standard "unknown" geographic
# CRS (unnamed datum, latitude/longitude axis order). basemap() clips and
# renders the map in the land layer's CRS, and that axis-swapped CRS makes the
# clip-box edges slant, leaving a thin uncovered wedge along the southern map
# edge. Normalise both layers to plain EPSG:4326 to fix it.
vb <- sf::st_transform(vb, 4326)
vl <- sf::st_transform(vl, 4326)

# vector_bathymetry() also polygonizes the "land" class added by
# estimate.land -- drop it, it belongs in vl.
vb <- vb[vb$depth != "land", ]
vb$depth <- droplevels(vb$depth)

save_fig(
  basemap(limits = north_sea,
          shapefiles = list(land = vl, glacier = NULL, bathy = vb),
          bathy.style = "pb"),
  "shapes_raster_vectorized.png"
)

# 3. Geonorge depth data: Oslo municipality ----------------------------------
gml <- Sys.getenv("GEONORGE_OSLO_GML")
gb <- geonorge_bathymetry(gml)
land_osl <- sf::st_read(gml, layer = "Landareal", quiet = TRUE)
bb <- sf::st_bbox(gb)
lims <- c(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"])

save_fig(
  basemap(limits = lims,
          shapefiles = list(land = land_osl, glacier = NULL, bathy = gb),
          bathy.style = "pb"),
  "shapes_geonorge_oslofjorden.png"
)

# 4. wcs_bathymetry(): Tromsø --------------------------------------------------
# This example is about making new shapefiles, so vectorise the WCS fetch
# instead of keeping it as a raw raster, and pair it with the premade
# "Europe" land set instead of the coarser shipped dd_land.
# wcs_bathymetry() always returns a continuous bathyRaster (it calls
# raster_bathymetry(ras, depths = NULL) internally), so the depth binning
# that raster_bathymetry() would otherwise do is repeated by hand here.
#
# The map is plotted in the "Europe" land set's CRS (EPSG:3035). A WCS fetch is
# a decimal-degree (EPSG:4326) box, and its straight lat/lon edges slant once
# reprojected to 3035, so a fetch box exactly equal to the map limits would
# leave uncovered slivers in the projected map's corners. Fetch a little wider
# than the area to display so the whole panel is covered.
tromso <- c(18, 20, 69.4, 69.9)    # area to display
fetch  <- c(17.5, 20.5, 69.3, 70)  # slightly wider fetch box
bathy <- wcs_bathymetry(fetch, source = "emodnet", verbose = FALSE)

breaks <- c(-Inf, 20, 50, 100, 200, Inf)
labels <- c("0-20", "20-50", "50-100", "100-200", "200-Inf")
rb <- list(
  raster = cut(bathy$raster, breaks, labels = labels),
  depth.invervals = data.frame(from = breaks[-length(breaks)], to = breaks[-1], interval = labels)
)
class(rb) <- "bathyRaster"
vb <- vector_bathymetry(rb, drop.crumbs = NULL)

# Reproject the bathymetry to the land set's CRS and clip the land to the
# bathymetry extent (both now in EPSG:3035, the plotted projection).
europe_land <- load_map_data(shapefile_list("Europe"))$land
vb <- sf::st_transform(vb, sf::st_crs(europe_land))
land <- sf::st_crop(sf::st_make_valid(europe_land), sf::st_bbox(vb))

save_fig(
  basemap(limits = tromso,
          shapefiles = list(land = land, glacier = NULL, bathy = vb),
          bathy.style = "pb"),
  "shapes_tromso_wcs.png"
)

message("Done.")
