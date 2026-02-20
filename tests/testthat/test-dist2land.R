test_that("dist2land works with data frame", {
  dt <- data.frame(lon = seq(-20, 80, length.out = 41), lat = 50:90)
  res <- dist2land(dt, verbose = FALSE)
  expect_true(is.data.frame(res))
  expect_true("ldist" %in% names(res))
})

test_that("dist2land works with binary option", {
  dt <- data.frame(lon = c(0, 10), lat = c(60, 70))
  res <- dist2land(dt, binary = TRUE, verbose = FALSE)
  expect_true(is.data.frame(res))
  # binary = TRUE returns ldist column as logical?
  # Manual says: "The distances are kilometers if binary = FALSE, otherwise logical (TRUE = the position is in the ocean, FALSE = the position is on land)."
  expect_type(res$ldist, "logical")
})

test_that("dist2land returns vector when bind = FALSE", {
  dt <- data.frame(lon = c(0, 10), lat = c(60, 70))
  res <- dist2land(dt, bind = FALSE, verbose = FALSE)
  expect_true(is.vector(res))
  expect_length(res, 2)
})
