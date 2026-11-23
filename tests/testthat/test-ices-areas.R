test_that("ices_areas is a valid, compact set of ICES statistical areas", {
  skip_if_not_installed("sf")
  a <- ggOceanMaps::ices_areas
  expect_s3_class(a, "sf")
  expect_equal(sf::st_crs(a)$epsg, 3995L)
  expect_named(a, c("Major_FA", "SubArea", "Area_Full", "geometry"))
  expect_equal(nrow(a), 66L)
  expect_false(anyDuplicated(a$Area_Full) > 0)
  expect_false(any(sf::st_is_empty(a)))
  suppressMessages(sf::sf_use_s2(FALSE))
  expect_true(all(sf::st_is_valid(a)))
  expect_lt(as.numeric(utils::object.size(a)) / 1024^2, 2)
})

test_that("27.4.a is bounded by the 62N parallel (Reg. EC 218/2009)", {
  skip_if_not_installed("sf")
  a <- ggOceanMaps::ices_areas
  g <- sf::st_transform(a[a$Area_Full == "27.4.a", ], 4326)
  expect_lte(max(sf::st_coordinates(g)[, 2]), 62 + 1e-6)
})

test_that("ices_areas polygons do not overlap and the CRS text is ASCII", {
  skip_if_not_installed("sf")
  a <- ggOceanMaps::ices_areas
  suppressMessages(sf::sf_use_s2(FALSE))
  ix <- suppressWarnings(sf::st_intersects(a))
  ov <- vapply(seq_len(nrow(a)), function(i) {
    j <- setdiff(ix[[i]], seq_len(i))
    if (!length(j)) return(0)
    sum(as.numeric(sf::st_area(suppressWarnings(sf::st_intersection(sf::st_geometry(a)[i], sf::st_geometry(a)[j])))))
  }, numeric(1))
  expect_lt(max(ov), 1e6) # m2, i.e. 1 km2
  expect_false(any(grepl("[^\\x01-\\x7F]", sf::st_crs(a)$wkt, perl = TRUE, useBytes = TRUE)))
})
