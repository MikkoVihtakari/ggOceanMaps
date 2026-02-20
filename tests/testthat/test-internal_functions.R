test_that("quiet works", {
  # Test that it suppresses output (stdout)
  expect_output(quiet(cat("This should not be printed")), NA)

  # Test that it returns the value
  expect_equal(quiet(1 + 1), 2)

  # Test that it does NOT suppress messages (stderr)
  expect_message(quiet(message("This is a message")), "This is a message")

  # Test that it propagates errors
  expect_error(quiet(stop("This is an error")), "This is an error")
})
