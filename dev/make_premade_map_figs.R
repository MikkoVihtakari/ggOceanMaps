# Generates the pre-rendered premade-map and fjord example figures embedded in
# vignettes/premade-maps.Rmd and vignettes/new-features-v3.Rmd. Not run during
# vignette compilation. Re-run manually when the maps or the underlying data
# change, then push ggOceanMapsLargeData.
#
#   Rscript dev/make_premade_map_figs.R

datapath <- Sys.getenv("GGOCEANMAPS_DATAPATH")
outdir   <- Sys.getenv("GGOCEANMAPS_LARGEDATA_DOCS")

if(!nzchar(datapath) || !dir.exists(datapath)) stop("Set GGOCEANMAPS_DATAPATH to the ggOceanMapsLargeData cache.")
if(!nzchar(outdir) || !dir.exists(outdir)) stop("Set GGOCEANMAPS_LARGEDATA_DOCS to the ggOceanMapsLargeData docs directory.")

options(ggOceanMaps.datapath = datapath)

devtools::load_all(".")
library(ggplot2)

save_fig <- function(p, name, w, h, dpi = 150) {
  ggsave(file.path(outdir, name), plot = p, width = w, height = h,
         dpi = dpi, bg = "white")
  message("saved ", name)
}

# Premade maps (vignettes/premade-maps.Rmd) ------------------------------
save_fig(basemap("DecimalDegree", bathymetry = TRUE, glaciers = TRUE),
         "DecimalDegree.png", w = 8, h = 4)
save_fig(basemap("ArcticStereographic", bathymetry = TRUE, glaciers = TRUE),
         "ArcticStereographic.png", w = 7, h = 7)
save_fig(basemap("AntarcticStereographic", bathymetry = TRUE, glaciers = TRUE),
         "AntarcticStereographic.png", w = 7, h = 7)
save_fig(basemap("Svalbard", bathymetry = TRUE, glaciers = TRUE),
         "Svalbard.png", w = 5, h = 8.2)
save_fig(basemap(limits = c(10.9, 12.65, 78.83, 79.12), bathymetry = TRUE,
                 shapefiles = "Svalbard", legends = FALSE, glaciers = TRUE),
         "Kongsfjorden.png", w = 7, h = 6.15)

# Fjord WCS examples (vignettes/new-features-v3.Rmd) ---------------------
save_fig(basemap(limits = c(10.9, 12.65, 78.83, 79.12),
                 bathy.style = "wemb", shapefiles = "Svalbard"),
         "kongsfjorden_wemb.png", w = 7, h = 6)
save_fig(basemap(limits = c(23.9, 26.5, 69.9, 71.15),
                 shapefiles = "Europe", bathy.style = "wemb"),
         "porsangerfjorden_wemb.png", w = 6.5, h = 7)

message("Done.")
