# get_depth() for data frames with explicit or guessed coordinate columns

test_that("get_depth() works with explicit lon and lat arguments", {
  skip_if_no_largedata()
  dt <- data.frame(x = c(10, 2), y = c(70, 70), id = 1:2)
  out <- get_depth(dt, lon = "x", lat = "y")
  expect_s3_class(out, "data.frame")
  expect_true(all(c("x", "y", "id", "depth") %in% names(out)))
  expect_type(out$depth, "double")
})

test_that("get_depth() gives the same depths with explicit and guessed coordinate columns", {
  skip_if_no_largedata()
  dt <- data.frame(lon = c(10, 2), lat = c(70, 70))
  expect_equal(
    get_depth(dt, lon = "lon", lat = "lat")$depth,
    get_depth(dt)$depth
  )
})

test_that("get_depth() keeps rows with missing coordinates as NA", {
  skip_if_no_largedata()
  dt <- data.frame(lon = c(10, NA, 2), lat = c(70, 70, 70))
  out <- get_depth(dt, lon = "lon", lat = "lat", bind = FALSE)
  expect_length(out, 3)
  expect_true(is.na(out[2]))
  expect_equal(out[c(1, 3)], get_depth(dt[-2, ], bind = FALSE))
})
