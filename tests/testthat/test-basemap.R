test_that("basemap works with integer limits", {
  p <- basemap(limits = 60)
  expect_s3_class(p, "ggplot")
})

test_that("basemap works with vector limits", {
  p <- basemap(limits = c(-20, 20, 40, 59))
  expect_s3_class(p, "ggplot")
})

test_that("basemap works with data limits", {
  dt <- data.frame(lon = c(-150, 150), lat = c(60, 90))
  p <- basemap(data = dt)
  expect_s3_class(p, "ggplot")
})

test_that("basemap works with bathymetry", {
  p <- basemap(limits = c(-60, -50, 60, 70), bathymetry = TRUE)
  expect_s3_class(p, "ggplot")
})

test_that("basemap works with rotate", {
  dt <- data.frame(lon = c(-160, 160, 160, -160), lat = c(80, 80, 60, 60))
  p <- basemap(data = dt, rotate = TRUE)
  expect_s3_class(p, "ggplot")
})
