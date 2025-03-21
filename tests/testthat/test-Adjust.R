# These help create consistent bl and bl_data objects for testing.

create_bl <- function(name, varnames, expectation, covariance) {
  bl(name = name, varnames = varnames, expectation = expectation, covariance = covariance)
}

create_bl_data <- function(name, varnames, values) {
  bl_data(name = name, varnames = varnames, values = values)
}

# --- Test Suite for bl_adjust ---

test_that("bl_adjust: Correct Adjustment with bl_data", {
  x <- create_bl(name = 'x',
                 varnames = c('A', 'B'),
                 expectation = c(0, 0),
                 covariance = matrix(c(1, 0.5, 0.5, 1), 2, 2))

  y <- create_bl_data(name = 'y',
                      varnames = c('B'),
                      values = c(1))

  xy <- bl_adjust(x, y)

  expect_equal(xy@name, "x_adj_y")
  expect_equal(xy@expectation, c(0.5, 1))
  expect_equal(xy@covariance, matrix(c(0.75, 0, 0, 0), 2, 2))

  # Test with different values and variable names
  x2 <- create_bl(name = "vars", varnames = c("X1", "X2", "X3"),
                  expectation = c(1, 2, 3),
                  covariance = matrix(c(1, .2, .3, .2, 1, .4, .3, .4, 1), 3, 3))
  y2 <- create_bl_data(name = "data", varnames = c("X2", "X3"), values = c(2.5, 3.5))
  xy2 <- bl_adjust(x2, y2)

  expect_equal(xy2@name, "vars_adj_data")
  # Manually calculated expected values (important to do this independently!)
  expected_expectation2 <- c(1.178571, 2.5, 3.5)
  expected_covariance2 <- matrix(c(0.9023810, 0, 0,
                                   0, 0, 0,
                                   0, 0, 0), 3, 3)
  expected_covariance2 <- (expected_covariance2 + t(expected_covariance2))/2
  expect_equal(xy2@expectation, expected_expectation2, tolerance = 1e-6) # Add tolerance for floating-point comparisons
  expect_equal(xy2@covariance, expected_covariance2, tolerance = 1e-6)

})

test_that("bl_adjust: Error Handling - Incorrect Input Types", {
  x <- create_bl("x", c("A", "B"), c(0, 0), diag(2))
  y_data <- create_bl_data("y", "B", 1)
  y_bl <- create_bl("y", "B", 1, matrix(1))
  invalid_input <- "not a bl object"

  expect_error(bl_adjust(invalid_input, y_data),
               "The class of x is character.  It should be bl.")
  expect_error(bl_adjust(x, invalid_input),
               "The class of y is character.  It should be bl or bl_data.")
  expect_error(bl_adjust(123, y_data),
               "The class of x is numeric.  It should be bl.") #Different invalid class
})


test_that("bl_adjust: Error Handling - Variable Mismatch", {
  x <- create_bl("x", c("A", "B"), c(0, 0), diag(2))
  y_data_wrong_vars <- create_bl_data("y", "C", 1)  # Variable C is not in x
  y_bl_wrong_vars <- create_bl("y", "C", 1, matrix(1))

  expect_error(bl_adjust(x, y_data_wrong_vars), "The variables to be adjusted are not in x.")
  expect_error(bl_adjust(x, y_bl_wrong_vars), "The variables to be adjusted are not in x.")
})

test_that("bl_adjust: Edge Cases - Single Variable", {
  x_single <- create_bl("x_single", "A", 1, matrix(2))  # Single variable
  y_data_single <- create_bl_data("y_single", "A", 2)
  xy_data_single <- bl_adjust(x_single, y_data_single)
  expect_equal(xy_data_single@expectation, 2)
  expect_equal(xy_data_single@covariance, matrix(0))

  y_bl_single <- create_bl("y_single", "A", 3, matrix(4))
  xy_bl_single <- bl_adjust(x_single, y_bl_single)
  expect_equal(xy_bl_single@expectation, 3, tolerance = 1e-7)
  expect_equal(xy_bl_single@covariance, matrix(4), tolerance = 1e-7)

})
