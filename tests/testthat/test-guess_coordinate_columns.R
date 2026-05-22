test_that("guess_coordinate_columns finds lon/lat", {
  res <- guess_coordinate_columns(data.frame(lon = 1, lat = 2))
  expect_equal(unname(res), c("lon", "lat"))
})

test_that("guess_coordinate_columns finds longitude/latitude", {
  res <- guess_coordinate_columns(
    data.frame(longitude = 1, latitude = 2)
  )
  expect_equal(unname(res), c("longitude", "latitude"))
})

test_that("guess_coordinate_columns finds x/y as fallback", {
  res <- guess_coordinate_columns(data.frame(x = 1, y = 2))
  expect_equal(unname(res), c("x", "y"))
})

test_that("guess_coordinate_columns handles dotted/spaced names", {
  res <- guess_coordinate_columns(
    data.frame(`long.dd` = 1, `lat.dd` = 2, check.names = FALSE)
  )
  expect_equal(unname(res["lat"]), "lat.dd")
})

test_that("guess_coordinate_columns errors on non-data.frame", {
  expect_error(guess_coordinate_columns(list(lon = 1, lat = 2)),
               "data\\.frame")
})
