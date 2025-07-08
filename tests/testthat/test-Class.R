# A valid covariance matrix for successful object creation
valid_cov <- matrix(c(1, 0.5, 0.2, 0.5, 1, 0.3, 0.2, 0.3, 1), 3, 3)

obj <- bl(
  name = "All",
  varnames = c("A", "B", "F"),
  expectation = c(0, 1, 6),
  covariance = valid_cov
)

odata <- bl_data(
  name = "Data",
  varnames = c("A", "B", "F"),
  values = c(1, 2, 10)
)

# --- Tests ---

test_that("Check object class is S4", {
  expect_true(isS4(obj))
  expect_true(isS4(odata))
})

test_that("Validity checks for bl class work as expected", {
  # Test for name length
  expect_error(bl(name = c("All", "Another"), varnames = c("A"),
                  expectation = c(0), covariance = 1),
    regexp = "Slot 'name' must have length 1, not 2."
  )

  # Test for unique variable names
  expect_error(bl(name = "All", varnames = c("A", "B", "A"),
                  expectation = c(0, 1, 6), covariance = valid_cov),
    regexp = "All variable names in 'varnames' must be unique."
  )

  # Test for correct number of expectations
  expect_error(bl(name = "All", varnames = c("A", "B", "F"),
                  expectation = c(0, 1), covariance = valid_cov),
    regexp = "Slot 'expectation' has length 2. It should match the number of variables: 3."
  )

  # NEW: Test for NAs in expectation
  expect_error(bl(name = "All", varnames = c("A", "B", "F"),
                  expectation = c(0, NA, 6), covariance = valid_cov),
    regexp = "The 'expectation' vector must not contain NA values."
  )

  # NEW: Test for NAs in covariance
  expect_error(bl(name = "All", varnames = c("A", "B", "F"),
                  expectation = c(0, 1, 6), covariance = matrix(c(1, 2, NA, 4),
                                                                2, 2)),
    regexp = "The 'covariance' matrix or value must not contain NA values."
  )

  # Test for covariance matrix dimensions
  expect_error(bl(name = "All", varnames = c("A", "B", "F"),
                  expectation = c(0, 1, 6), covariance = matrix(1, 2, 2)),
    regexp = "The covariance matrix has dimensions \\(2, 2\\). It must be a square matrix with dimensions \\(3, 3\\)."
  )

  # Test for covariance matrix symmetry
  asymmetric_cov <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), 3, 3)
  expect_error(bl(name = "All", varnames = c("A", "B", "F"),
                  expectation = c(0, 1, 6), covariance = asymmetric_cov),
    regexp = "The covariance matrix is not symmetric."
  )

  # NEW: Test for positive semi-definiteness
  non_psd_cov <- matrix(c(1, 2, 2, 1), 2, 2) # Symmetric, but has a negative eigenvalue
  expect_error(bl(name = "All", varnames = c("A", "B"),
                  expectation = c(0, 1), covariance = non_psd_cov),
    regexp = "The covariance matrix is not positive semi-definite."
  )
})

test_that("Scalar covariance validity checks for bl class", {
  # Test valid scalar case
  expect_no_error(bl(name = "Single", varnames = "A",
                     expectation = 1, covariance = 4))

  # Test for negative scalar variance
  expect_error(bl(name = "Single", varnames = "A",
                  expectation = 1, covariance = -2),
    regexp = "The scalar covariance \\(variance\\) must be non-negative."
  )

  # Test for vector covariance when only one variable is present
  expect_error(bl(name = "Single", varnames = "A",
                  expectation = 1, covariance = c(1, 2)),
    regexp = "For a single variable, covariance should be a single numeric value"
  )
})


test_that("Validity checks for bl_data class", {
  # Test for name length
  expect_error(bl_data(name = c("Data", "Another"),
                       varnames = c("A", "B", "F"),
                       values = c(1, 2, 10)),
    regexp = paste0("Slot 'name' must have length 1, not 2.")
  )

  # Test for unique variable names
  expect_error(bl_data(name = "Data",
                       varnames = c("A", "B", "A"),
                       values = c(1, 2, 10)),
    regexp = "All variable names in varnames must be unique."
  )

  # Test for correct number of values
  expect_error(bl_data(name = "Data", varnames = c("A", "B", "F"),
                       values = c(1, 2)),
    regexp = "Slot 'values' has length 2. It should match the number of variables: 3."
  )
})
