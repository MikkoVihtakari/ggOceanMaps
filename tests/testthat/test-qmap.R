test_that("qmap works with basic data frame", {
  dt <- data.frame(lon = c(-100, -80, -60), lat = c(10, 25, 40), var = c("a", "a", "b"))
  p <- qmap(dt)
  expect_s3_class(p, "ggplot")
})

test_that("qmap works with color aesthetic", {
  dt <- data.frame(lon = c(-100, -80, -60), lat = c(10, 25, 40), var = c("a", "a", "b"))
  p <- qmap(dt, color = I("blue"))
  expect_s3_class(p, "ggplot")
})

test_that("qmap works with mapped color", {
  dt <- data.frame(lon = c(-100, -80, -60), lat = c(10, 25, 40), var = c("a", "a", "b"))
  p <- qmap(dt, color = var)
  expect_s3_class(p, "ggplot")
})

test_that("qmap works with rotation", {
  dt <- data.frame(lon = c(-80, -80, -50, -50), lat = c(65, 80, 80, 65))
  p <- qmap(dt, rotate = TRUE)
  expect_s3_class(p, "ggplot")
})
