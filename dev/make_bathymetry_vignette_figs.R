# Generates the pre-rendered example figures used in vignettes/bathymetry.Rmd.
# Not run during vignette compilation. Re-run manually when the bathymetry
# styles or the underlying data change, then push ggOceanMapsLargeData.
#
#   Rscript dev/make_bathymetry_vignette_figs.R

datapath <- Sys.getenv("GGOCEANMAPS_DATAPATH")
userpath <- Sys.getenv("GGOCEANMAPS_USERPATH")
outdir <- Sys.getenv("GGOCEANMAPS_LARGEDATA_DOCS")

if(!nzchar(datapath) || !dir.exists(datapath)) stop("Set GGOCEANMAPS_DATAPATH to the ggOceanMapsLargeData cache.")
if(!nzchar(userpath) || !file.exists(userpath)) stop("Set GGOCEANMAPS_USERPATH to a local GEBCO/ETOPO/IBCAO raster.")
if(!nzchar(outdir) || !dir.exists(outdir)) stop("Set GGOCEANMAPS_LARGEDATA_DOCS to the ggOceanMapsLargeData docs directory.")

options(ggOceanMaps.datapath = datapath, ggOceanMaps.userpath = userpath)

devtools::load_all(".")
library(ggplot2)

save_fig <- function(p, name, w = 6, h = 5, dpi = 150) {
  ggsave(file.path(outdir, name), plot = p, width = w, height = h,
         dpi = dpi, bg = "white")
  message("saved ", name)
}

nnorway <- c(11, 16, 67.3, 68.6)

# ggOceanMapsLargeData rasters / contours --------------------------------
save_fig(basemap(nnorway, bathy.style = "rcb"), "bathy_rcb.png")
save_fig(basemap(nnorway, bathy.style = "pb"),  "bathy_pb.png")
save_fig(basemap(nnorway, bathy.style = "cb"),  "bathy_cb.png")

# User raster (GEBCO) ----------------------------------------------------
save_fig(basemap(nnorway, bathy.style = "rub"), "bathy_rub.png")

# Web Coverage Services --------------------------------------------------
save_fig(basemap(c(2, 3, 54, 55), bathy.style = "wemb"), "bathy_wemb.png")
save_fig(basemap(c(-160, -154, 18, 23), bathy.style = "wceb"), "bathy_wceb.png")

# Custom depth bins example (vignettes/adding-graphical-elements.Rmd) -----
local({
  rb <- raster_bathymetry(
    getOption("ggOceanMaps.userpath"),
    depths = c(50, 100, 250, 500, 1000, 2000),
    boundary = c(11, 16, 67.3, 68.6),
    estimate.land = TRUE,
    verbose = FALSE
  )
  # Land from the same grid so it lines up with the bathymetry.
  land <- sf::st_transform(vector_land(rb), 4326)
  # Drop the land class so the raster shows depth bins only.
  v <- rb$raster[[1]]
  v[as.character(v) == "land"] <- NA
  rb$raster[[1]] <- droplevels(v)
  save_fig(
    basemap(limits = c(11, 16, 67.3, 68.6),
            shapefiles = list(land = land, glacier = NULL, bathy = rb),
            bathymetry = TRUE, grid.col = NA) +
      scale_fill_brewer("Depth (m)", palette = "YlGnBu", na.value = "white"),
    "custom_depth_bins.png", w = 6, h = 4.5
  )
})

message("Done.")
