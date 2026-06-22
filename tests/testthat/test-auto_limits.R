test_that("auto_limits returns expected structure for polar data", {
  res <- auto_limits(
    data = expand.grid(lon = c(-120, 180, 120), lat = c(60, 60, 80))
  )
  expect_type(res, "list")
  expect_true("ddLimits" %in% names(res) || "decLimits" %in% names(res) ||
              any(grepl("limits", names(res), ignore.case = TRUE)))
})

test_that("auto_limits handles sf input", {
  pts <- sf::st_sfc(
    sf::st_multipoint(as.matrix(
      data.frame(lon = c(-20, 20), lat = c(50, 65))
    )),
    crs = 4326
  )
  expect_no_error(auto_limits(pts))
})

test_that("auto_limits honors expand.factor", {
  d <- data.frame(lon = c(-10, 10), lat = c(55, 65))
  bare <- auto_limits(d, expand.factor = NULL)
  wide <- auto_limits(d, expand.factor = 2)
  # Expanded should differ from bare
  expect_false(identical(bare, wide))
})
