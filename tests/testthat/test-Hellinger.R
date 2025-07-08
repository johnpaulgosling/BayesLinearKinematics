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
