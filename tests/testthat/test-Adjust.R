# These functions help create consistent bl and bl_data objects for testing.

create_bl <- function(name, varnames, expectation, covariance) {
  # Ensure dimnames match varnames if covariance is provided
  if (!is.null(covariance) && is.matrix(covariance)) {
    dimnames(covariance) <- list(varnames, varnames)
  }
  bl(name = name, varnames = varnames, expectation = expectation, covariance = covariance)
}

create_bl_data <- function(name, varnames, values) {
  bl_data(name = name, varnames = varnames, values = values)
}

# --- Test Suite for bl_adjust ---

test_that("bl_adjust: Correct Adjustment with bl_data (Manual Example 1)", {
  x <- create_bl(name = 'x',
                 varnames = c('A', 'B'),
                 expectation = c(0, 0),
                 covariance = matrix(c(1, 0.5, 0.5, 1), 2, 2))

  y <- create_bl_data(name = 'y',
                      varnames = c('B'),
                      values = c(1))

  xy <- bl_adjust(x, y)

  # Check results (using default adjusted name from your original test)
  expect_equal(xy@name, "x_adj_y") # Using likely default from bl_adjust
  # Or expect_equal(xy@name, "x_adj_y") # If you expect specific name construction
  expect_equal(xy@expectation, c(0.5, 1))
  expect_equal(xy@covariance, matrix(c(0.75, 0, 0, 0), 2, 2, dimnames=list(c('A','B'), c('A','B'))))
  expect_equal(xy@varnames, c('A', 'B')) # Added check for varnames

})

test_that("bl_adjust: Correct Adjustment with bl_data (Manual Example 2)", {
  # Test with different values and variable names
  x2 <- create_bl(name = "vars", varnames = c("X1", "X2", "X3"),
                  expectation = c(1, 2, 3),
                  covariance = matrix(c(1, .2, .3, .2, 1, .4, .3, .4, 1), 3, 3))
  y2 <- create_bl_data(name = "data", varnames = c("X2", "X3"), values = c(2.5, 3.5))
  xy2 <- bl_adjust(x2, y2)

  expect_equal(xy2@name, "vars_adj_data") # Using likely default
  # Or expect_equal(xy2@name, "vars_adj_data") # If you expect specific name construction

  # Manually calculated expected values (important to do this independently!)
  # Note: The original calculation for expected_covariance2 seemed incorrect
  # for a standard Bayes Linear update. Recalculating based on formulae:
  # E_adj = E_x + Cov_xy * Var_y^-1 * (y - E_y)
  # Var_adj = Var_x - Cov_xy * Var_y^-1 * Cov_yx
  Ex <- c(1, 2, 3)
  Vx <- matrix(c(1, .2, .3, .2, 1, .4, .3, .4, 1), 3, 3)
  dimnames(Vx) <- list(c("X1", "X2", "X3"), c("X1", "X2", "X3"))
  Ey <- c(2, 3)
  Vy <- Vx[c("X2", "X3"), c("X2", "X3")]
  Cxy <- Vx[c("X1", "X2", "X3"), c("X2", "X3")]
  y_obs <- c(2.5, 3.5)
  InvVy <- solve(Vy)

  expected_expectation2 <- Ex + Cxy %*% InvVy %*% (y_obs - Ey)
  expected_covariance2 <- Vx - Cxy %*% InvVy %*% t(Cxy)

  # Add dimnames for comparison
  dimnames(expected_covariance2) <- list(c("X1","X2","X3"), c("X1","X2","X3"))

  expect_equal(xy2@expectation, c(expected_expectation2), tolerance = 1e-6) # Compare vector to vector
  expect_equal(xy2@covariance, expected_covariance2, tolerance = 1e-6)
  expect_equal(xy2@varnames, c('X1', 'X2', 'X3')) # Added check for varnames
})


# --- Tests Based on Vignette Examples ---

test_that("bl_adjust: Vignette example - Adjustment with bl_data (Section 3)", {
  # Setup objects based on vignette Section 1 & 2
  bl_prior_v <- create_bl(name = 'Prior Beliefs',
                          varnames = c('x', 'y', 'z'),
                          expectation = c(1, 2, 3),
                          covariance = matrix(c(1.0, 0.5, 0.2,
                                                0.5, 1.0, 0.5,
                                                0.2, 0.5, 1.0),
                                              nrow = 3, ncol = 3)
  )

  observed_data_v <- create_bl_data(name = 'Observations',
                                    varnames = c('x', 'y'),
                                    values = c(1.1, 2.1))

  # Perform the adjustment
  bl_adjusted_data_v <- bl_adjust(x = bl_prior_v, y = observed_data_v)

  # Expected values from vignette print(..., digits = 3)
  expected_exp <- c(1.1,2.1,3.047)
  expected_cov <- matrix(c(rep(0,8), 0.747),
                         nrow = 3, ncol = 3,
                         dimnames = list(c('x','y','z'), c('x','y','z'))) # Add dimnames

  # Check adjusted expectation
  expect_equal(bl_adjusted_data_v@expectation, expected_exp, tolerance = 0.01)

  # Check adjusted covariance
  expect_equal(bl_adjusted_data_v@covariance, expected_cov, tolerance = 0.01)

  # Check variable names are preserved
  expect_equal(bl_adjusted_data_v@varnames, c('x', 'y', 'z'))

  # Check name is automatically generated
  expect_equal(bl_adjusted_data_v@name, "Prior Beliefs_adj_Observations") # Check default name
})


test_that("bl_adjust: Vignette example - Adjustment with bl (Kinematics, Section 4)", {
  # Use bl_prior_v from previous test setup
  bl_prior_v <- create_bl(name = 'Prior Beliefs',
                          varnames = c('x', 'y', 'z'),
                          expectation = c(1, 2, 3),
                          covariance = matrix(c(1.0, 0.5, 0.2,
                                                0.5, 1.0, 0.5,
                                                0.2, 0.5, 1.0),
                                              nrow = 3, ncol = 3)
  )

  # Kinematic info (Section 4)
  bl_info_v <- create_bl(name = "Sensor Info",
                         varnames = c('y', 'z'),
                         expectation = c(2.2, 2.8),
                         covariance = matrix(c(0.5, 0.1,
                                               0.1, 0.6), nrow=2, ncol=2))

  # Perform the adjustment
  bl_adjusted_kinematics_v <- bl_adjust(x = bl_prior_v, y = bl_info_v)

  # Expected values from vignette print(..., digits = 3)
  expected_exp <- c(1.12,2.2,2.80)
  expected_cov <- matrix(c(0.884, 0.26, 0.013,
                           0.26, 0.5, 0.1,
                           0.013, 0.10, 0.6),
                         nrow = 3, ncol = 3,
                         dimnames = list(c('x','y','z'), c('x','y','z'))) # Add dimnames

  # Check adjusted expectation
  expect_equal(bl_adjusted_kinematics_v@expectation, expected_exp, tolerance = 0.01)

  # Check adjusted covariance
  expect_equal(bl_adjusted_kinematics_v@covariance, expected_cov, tolerance = 0.01)

  # Check variable names are preserved
  expect_equal(bl_adjusted_kinematics_v@varnames, c('x', 'y', 'z'))

  # Check name is automatically generated
  expect_equal(bl_adjusted_kinematics_v@name, "Prior Beliefs_adj_Sensor Info") # Check default name
})


# --- Error Handling Tests ---
test_that("bl_adjust: Error Handling - Incorrect Input Types", {
  x <- create_bl("x", c("A", "B"), c(0, 0), diag(2))
  y_data <- create_bl_data("y", "B", 1)
  y_bl <- create_bl("y", "B", 1, matrix(1))
  invalid_input <- "not a bl object"

  # Using more general error message checking unless specific messages are guaranteed
  expect_error(bl_adjust(invalid_input, y_data), class="simpleError") # Or regexp matching part of msg
  expect_error(bl_adjust(x, invalid_input), class="simpleError")
  expect_error(bl_adjust(123, y_data), class="simpleError")
})


test_that("bl_adjust: Error Handling - Variable Mismatch", {
  x <- create_bl("x", c("A", "B"), c(0, 0), diag(2))
  y_data_wrong_vars <- create_bl_data("y", "C", 1)  # Variable C is not in x
  y_bl_wrong_vars <- create_bl("y", "C", 1, matrix(1))

  # Using more general error message checking
  expect_error(bl_adjust(x, y_data_wrong_vars), class="simpleError")
  expect_error(bl_adjust(x, y_bl_wrong_vars), class="simpleError")
})


# Consider adding tests for non-invertible covariance matrices if applicable
# test_that("bl_adjust: Handles non-invertible matrices", { ... })
