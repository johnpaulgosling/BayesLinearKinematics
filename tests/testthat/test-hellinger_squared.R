# --- Helper Objects for Testing ---

# Basic valid bl object
bl1 <- bl(
  name = "Dist A",
  varnames = c("v1", "v2"),
  expectation = c(1, 2),
  covariance = matrix(c(1, 0.5, 0.5, 1), 2, 2)
)

bl2 <- bl(
  name = "Dist B",
  varnames = c("v1", "v2"),
  expectation = c(1.5, 2.5),
  covariance = matrix(c(1.2, 0.6, 0.6, 1.5), 2, 2)
)

# Identical to bl1
bl1_identical <- bl(
  name = "Dist A Clone",
  varnames = c("v1", "v2"),
  expectation = c(1, 2),
  covariance = matrix(c(1, 0.5, 0.5, 1), 2, 2)
)

# Different variable names
bl_diff_vars <- bl(
  name = "Dist C",
  varnames = c("x1", "x2"), # Different names
  expectation = c(1, 2),
  covariance = matrix(c(1, 0.5, 0.5, 1), 2, 2)
)

# Overlapping but not identical variable names
bl_overlap_vars <- bl(
  name = "Dist D",
  varnames = c("v1", "v3"), # v2 missing, v3 added
  expectation = c(1, 3),
  covariance = matrix(c(1, 0.2, 0.2, 1), 2, 2)
)

# Same variables as bl1 but different order
bl1_reordered <- bl(
  name = "Dist A Reordered",
  varnames = c("v2", "v1"), # Order swapped
  expectation = c(2, 1), # Order swapped
  covariance = matrix(c(1, 0.5, 0.5, 1), 2, 2)[c(2, 1), c(2, 1)]
) # Rows/cols swapped

# Simple 1D cases
bl_1d_a <- bl(name = "1D A", varnames = "z", expectation = 0, covariance = 1)
bl_1d_b <- bl(name = "1D B", varnames = "z", expectation = 1, covariance = 2)


# --- Test Cases ---

test_that("Input validation works correctly", {
  # Non-bl objects
  expect_error(hellinger_squared(x = 1, y = bl1),
    regexp = "The class of x is numeric.  It should be bl."
  )
  expect_error(hellinger_squared(x = bl1, y = list()),
    regexp = "The class of y is list.  It should be bl."
  )
  expect_error(hellinger_squared(x = matrix(1), y = matrix(1)),
    regexp = "The class of x is matrix.  It should be bl."
  ) # Will report x first

  # Mismatched variable names
  expect_error(hellinger_squared(x = bl1, y = bl_diff_vars),
    regexp = "Some of the variables in y are not in x."
  ) # Checks y vs x first
  expect_error(hellinger_squared(x = bl_diff_vars, y = bl1),
    regexp = "Some of the variables in y are not in x."
  )

  # Overlapping but not identical variable names
  expect_error(hellinger_squared(x = bl1, y = bl_overlap_vars),
    regexp = "Some of the variables in y are not in x."
  )
  expect_error(hellinger_squared(x = bl_overlap_vars, y = bl1),
    regexp = "Some of the variables in y are not in x."
  )
})

test_that("Calculation is correct for identical distributions", {
  # Distance between identical objects should be 0
  expect_equal(hellinger_squared(bl1, bl1), 0, tolerance = 1e-9)
  expect_equal(hellinger_squared(bl1, bl1_identical), 0, tolerance = 1e-9)
})

test_that("Calculation handles variable reordering", {
  # Distance between bl1 and its reordered version should be 0
  # This relies on bl_subset working correctly inside Hellinger_squared
  expect_equal(hellinger_squared(bl1, bl1_reordered), 0, tolerance = 0.01)
  expect_equal(hellinger_squared(bl1_reordered, bl1), 0, tolerance = 0.01)

  # Calculate between bl1 and bl2, and bl1_reordered and bl2
  # The results should be the same (testing interaction with bl_subset)
  res12 <- hellinger_squared(bl1, bl2)
  res_reordered_12 <- hellinger_squared(bl1_reordered, bl2)
  expect_equal(res12 - res_reordered_12, 0, tolerance = 0.01)
})

test_that("Calculation gives plausible result for different distributions", {
  # Just checking it runs and gives a number between 0 and 1 for a standard case
  result <- hellinger_squared(bl1, bl2)
  expect_type(result, "double")
  expect_true(result >= 0 - 1e-9) # Allow for tiny numerical error below 0
  expect_true(result <= 1 + 1e-9) # Allow for tiny numerical error above 1
})

# --- Additional Tests ---

test_that("hellinger_squared: Univariate case", {
  bl_1d_a <- bl(
    name = "1D A",
    varnames = "x",
    expectation = 0,
    covariance = 1
  )
  bl_1d_b <- bl(
    name = "1D B",
    varnames = "x",
    expectation = 1,
    covariance = 2
  )
  
  result <- hellinger_squared(bl_1d_a, bl_1d_b)
  
  expect_type(result, "double")
  expect_true(result >= 0)
  expect_true(result <= 1)
  expect_true(result > 0) # Should be non-zero for different distributions
})

test_that("hellinger_squared: Identical univariate distributions", {
  bl_1d <- bl(
    name = "1D",
    varnames = "z",
    expectation = 5,
    covariance = 2
  )
  
  result <- hellinger_squared(bl_1d, bl_1d)
  
  expect_equal(result, 0, tolerance = 1e-10)
})

test_that("hellinger_squared: Symmetry property", {
  result_xy <- hellinger_squared(bl1, bl2)
  result_yx <- hellinger_squared(bl2, bl1)
  
  # Distance should be symmetric
  expect_equal(result_xy, result_yx, tolerance = 1e-10)
})

test_that("hellinger_squared: Large number of variables", {
  n <- 5
  bl_a <- bl(
    name = "A",
    varnames = paste0("v", 1:n),
    expectation = rep(0, n),
    covariance = diag(n)
  )
  bl_b <- bl(
    name = "B",
    varnames = paste0("v", 1:n),
    expectation = rep(1, n),
    covariance = diag(n) * 2
  )
  
  result <- hellinger_squared(bl_a, bl_b)
  
  expect_type(result, "double")
  expect_true(result >= 0)
  expect_true(result <= 1)
})

test_that("hellinger_squared: Same expectation, different covariance", {
  bl_a <- bl(
    name = "A",
    varnames = c("x", "y"),
    expectation = c(0, 0),
    covariance = matrix(c(1, 0, 0, 1), 2, 2)
  )
  bl_b <- bl(
    name = "B",
    varnames = c("x", "y"),
    expectation = c(0, 0),
    covariance = matrix(c(2, 0, 0, 2), 2, 2)
  )
  
  result <- hellinger_squared(bl_a, bl_b)
  
  expect_type(result, "double")
  expect_true(result >= 0)
  expect_true(result < 1) # Should be less than 1 since not completely different
})

test_that("hellinger_squared: Different expectation, same covariance", {
  bl_a <- bl(
    name = "A",
    varnames = c("x", "y"),
    expectation = c(0, 0),
    covariance = matrix(c(1, 0.5, 0.5, 1), 2, 2)
  )
  bl_b <- bl(
    name = "B",
    varnames = c("x", "y"),
    expectation = c(2, 3),
    covariance = matrix(c(1, 0.5, 0.5, 1), 2, 2)
  )
  
  result <- hellinger_squared(bl_a, bl_b)
  
  expect_type(result, "double")
  expect_true(result >= 0)
  expect_true(result <= 1)
  expect_true(result > 0) # Should be positive due to different expectations
})

test_that("hellinger_squared: Works with correlated variables", {
  bl_corr_a <- bl(
    name = "Corr A",
    varnames = c("x", "y", "z"),
    expectation = c(1, 2, 3),
    covariance = matrix(c(
      1, 0.7, 0.5,
      0.7, 1, 0.6,
      0.5, 0.6, 1
    ), 3, 3)
  )
  bl_corr_b <- bl(
    name = "Corr B",
    varnames = c("x", "y", "z"),
    expectation = c(1.5, 2.5, 3.5),
    covariance = matrix(c(
      1.2, 0.8, 0.6,
      0.8, 1.3, 0.7,
      0.6, 0.7, 1.4
    ), 3, 3)
  )
  
  result <- hellinger_squared(bl_corr_a, bl_corr_b)
  
  expect_type(result, "double")
  expect_true(result >= 0)
  expect_true(result <= 1)
})

test_that("hellinger_squared: Returns scalar value", {
  result <- hellinger_squared(bl1, bl2)
  
  expect_length(result, 1)
  expect_type(result, "double")
})

test_that("hellinger_squared: Error with partially overlapping variables", {
  bl_partial_a <- bl(
    name = "Partial A",
    varnames = c("x", "y", "z"),
    expectation = c(1, 2, 3),
    covariance = diag(3)
  )
  bl_partial_b <- bl(
    name = "Partial B",
    varnames = c("x", "y", "w"),
    expectation = c(1, 2, 4),
    covariance = diag(3)
  )
  
  expect_error(
    hellinger_squared(bl_partial_a, bl_partial_b),
    regexp = "Some of the variables"
  )
})
