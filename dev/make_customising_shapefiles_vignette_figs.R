# Generates the pre-rendered example figures used in
# vignettes/customising-shapefiles.Rmd. Not run during vignette compilation.
# Re-run manually when the examples change, then push ggOceanMapsLargeData.
#
#   Rscript dev/make_customising_shapefiles_vignette_figs.R
#
# Two examples need a local file that is not part of this repo. Set these as
# environment variables (e.g. in ~/.Renviron) rather than hardcoding a path:
#   GGOCEANMAPS_USERPATH - a GEBCO/ETOPO/IBCAO grid (raster_bathymetry example)
#   GEONORGE_OSLO_GML     - an unzipped Geonorge "Dybdedata" .gml file for an
#                            Oslofjorden municipality, from
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
sw_norway <- c(10, 11, 59, 60)
rb <- raster_bathymetry(
  getOption("ggOceanMaps.userpath"),
  depths = c(50, 200, 500, 1000, 2000, 4000),
  boundary = sw_norway,
  estimate.land = TRUE,
  verbose = FALSE
)
vb <- vector_bathymetry(rb, drop.crumbs = 10)
vl <- vector_land(rb, drop.crumbs = 10)

# vector_bathymetry() also polygonizes the "land" class added by
# estimate.land -- drop it, it belongs in vl.
vb <- vb[vb$depth != "land", ]
vb$depth <- droplevels(vb$depth)

save_fig(
  basemap(limits = sw_norway,
          shapefiles = list(land = vl, glacier = NULL, bathy = vb),
          bathy.style = "pb"),
  "shapes_raster_vectorized.png"
)

# 3. Geonorge depth data: Oslofjorden -----------------------------------------
gml <- Sys.getenv("GEONORGE_OSLO_GML")
gb <- geonorge_bathymetry(gml)
land_osl <- sf::st_read(gml, layer = "Landareal", quiet = TRUE)
land_osl <- sf::st_make_valid(land_osl["område"])
bb <- sf::st_bbox(gb)
lims <- c(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"])

# Geonorge's native depth classes are fine-grained (here 32 levels); collapse
# them to a handful of bins for a readable legend.
gb$depth <- cut(as.numeric(as.character(gb$depth)),
                breaks = c(0, 5, 10, 20, 50, Inf),
                labels = c("0-5", "5-10", "10-20", "20-50", "50+"))

save_fig(
  basemap(limits = lims,
          shapefiles = list(land = land_osl, glacier = NULL, bathy = gb),
          bathy.style = "pb") +
    scale_fill_brewer("Depth (m)", palette = "Blues", direction = -1),
  "shapes_geonorge_oslofjorden.png"
)

# 4. Vectorising a small live WCS tile: Tromsø -------------------------------
# wcs_bathymetry() discards land; fetching the raw elevation grid instead lets
# raster_bathymetry()/vector_bathymetry()/vector_land() build matching layers.
tromso <- c(18.85, 19.05, 69.60, 69.72)  # xmin, xmax, ymin, ymax

url <- sprintf(
  paste0(
    "https://ows.emodnet-bathymetry.eu/wcs?SERVICE=WCS&VERSION=2.0.1",
    "&REQUEST=GetCoverage&CoverageId=emodnet__mean",
    "&SUBSET=Lat(%f,%f)&SUBSET=Long(%f,%f)&FORMAT=image/tiff"
  ),
  tromso[3], tromso[4], tromso[1], tromso[2]
)
tif <- tempfile(fileext = ".tif")
download.file(url, tif, mode = "wb", quiet = TRUE)

rb2 <- raster_bathymetry(tif, depths = c(10, 20, 50), estimate.land = TRUE, verbose = FALSE)
vb2 <- vector_bathymetry(rb2, drop.crumbs = 0.01)
vl2 <- vector_land(rb2, drop.crumbs = 0.01)

# vector_bathymetry() also polygonizes the "land" class -- drop it, it
# belongs in vl2.
vb2 <- vb2[vb2$depth != "land", ]
vb2$depth <- droplevels(vb2$depth)

save_fig(
  basemap(limits = tromso,
          shapefiles = list(land = vl2, glacier = NULL, bathy = vb2),
          bathy.style = "pb") +
    scale_fill_brewer("Depth (m)", palette = "Blues", direction = -1),
  "shapes_tromso_wcs.png"
)

message("Done.")
