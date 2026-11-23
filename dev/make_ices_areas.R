# Build data/ices_areas.rda: ICES statistical areas (FAO Major Fishing Area 27) in EPSG:3995.
#
# Source: the `icesAreas` polygons of BioticExplorerServer (match the ICES legal definitions;
# they reach over adjacent land, which is fine because ggOceanMaps draws land on top).
# The only edit is removing the part that extends over Russian land, which would otherwise
# be drawn as a large area of "sea" when land is not drawn on top.
#
# Run: Rscript dev/make_ices_areas.R   (needs BioticExplorerServer, rnaturalearth, sf, dplyr)

suppressMessages({library(sf); library(dplyr)})
sf_use_s2(FALSE)

out <- Sys.getenv("ICES_OUT", "data/ices_areas.rda")
crs <- 3995
seg <- 0.25      # max segment length (deg) before projecting, keeps parallels curved
erode <- 10000   # m; Russian land is eroded by this to favor the land side
tol <- 3000      # m; simplification of the cut line

# Insert vertices so that no segment exceeds `max` degrees (planar, lon/lat); keeps
# parallels curved after projecting. Base R, because st_segmentize() needs lwgeom.
densify_ring <- function(m, max) {
  do.call(rbind, lapply(seq_len(nrow(m) - 1), function(i) {
    n <- max(1, ceiling(max(abs(m[i + 1, 1:2] - m[i, 1:2])) / max))
    t <- seq(0, 1, length.out = n + 1)[-(n + 1)]
    cbind(m[i, 1] + t * (m[i + 1, 1] - m[i, 1]), m[i, 2] + t * (m[i + 1, 2] - m[i, 2]))
  })) |> rbind(m[nrow(m), 1:2, drop = FALSE])
}
densify <- function(g, max) {
  st_sfc(lapply(g, function(x) {
    x <- st_cast(x, "MULTIPOLYGON")
    st_multipolygon(lapply(x, function(poly) lapply(poly, densify_ring, max = max)))
  }), crs = st_crs(g))
}

bes <- BioticExplorerServer::icesAreas
areas <- bes |>
  st_make_valid() |>
  group_by(Major_FA, SubArea, Area_Full) |>
  summarise(.groups = "drop") |>
  st_make_valid()
st_geometry(areas) <- densify(st_geometry(areas), seg)
areas <- areas |> st_transform(crs) |> st_make_valid()
# keep the area order of the source
areas <- areas[match(unique(bes$Area_Full), areas$Area_Full), ]

russia <- rnaturalearth::ne_countries(scale = 10, country = "Russia", returnclass = "sf") |>
  st_geometry() |> st_make_valid() |> st_transform(crs) |> st_union() |>
  st_buffer(-erode) |> st_simplify(dTolerance = tol) |> st_make_valid()

st_geometry(areas) <- st_difference(st_geometry(areas), russia) |> st_make_valid()
areas <- areas |> filter(!st_is_empty(geometry))

ices_areas <- areas |>
  mutate(across(c(Major_FA, SubArea, Area_Full), as.character)) |>
  as_tibble() |> st_as_sf()

# The EPSG:3995 WKT contains a degree sign (in the usage area text), which gives an R CMD
# check warning for non-ASCII data. Replace it; the CRS itself is unchanged.
st_crs(ices_areas) <- st_crs(gsub("\u00b0", " deg ", st_crs(crs)$wkt, fixed = TRUE))

save(ices_areas, file = out, compress = "xz", version = 2)
message("Saved ", out, ": ", round(file.size(out) / 1024), " kB, ",
        nrow(ices_areas), " areas")
