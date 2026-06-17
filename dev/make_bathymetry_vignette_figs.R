# Generates the pre-rendered example figures used in vignettes/bathymetry.Rmd.
# Not run during vignette compilation. Re-run manually when the bathymetry
# styles or the underlying data change, then push ggOceanMapsLargeData.
#
#   Rscript dev/make_bathymetry_vignette_figs.R

options(
  ggOceanMaps.datapath = "~/Documents/ggOceanMapsLargeData",
  ggOceanMaps.userpath = "/Users/a22357/Downloads/gebco_2025/GEBCO_2025.nc"
)

devtools::load_all(".")
library(ggplot2)

outdir <- "/Users/a22357/ownCloud/Workstuff/R/Github/ggOceanMapsLargeData/docs"

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

message("Done.")
