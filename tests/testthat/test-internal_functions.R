test_that("LS works as expected", {
  # Test with single numeric values
  expect_equal(LS(2.13), 1)
  expect_equal(LS(4.26), 2)
  expect_equal(LS(0), 0)
  expect_equal(LS(-2.13), -1)

  # Test with numeric vectors
  expect_equal(LS(c(2.13, 4.26, 0)), c(1, 2, 0))

  # Test with edge cases
  expect_true(is.na(LS(NA)))
  expect_true(is.nan(LS(NaN)))
  expect_equal(LS(Inf), Inf)
  expect_equal(LS(-Inf), -Inf)

  # Test exact mathematical formula
  x <- 10.5
  expect_equal(LS(x), x / 2.13)
})

test_that("LS handles non-numeric input", {
  # In R, non-numeric input to division usually produces an error or NA depending on how it's handled.
  # Since LS is x/2.13, it should behave like standard R division.
  expect_error(LS("a"))
})
