# Tests for bl class and its methods

# --- Helper Objects for Testing ---

# Valid covariance matrices
valid_cov_2x2 <- matrix(c(1, 0.5, 0.5, 1), 2, 2)
valid_cov_3x3 <- matrix(c(1, 0.5, 0.2, 0.5, 1, 0.3, 0.2, 0.3, 1), 3, 3)

# --- Tests for bl Constructor ---

test_that("bl creates S4 class object", {
  obj <- bl(
    name = "S4 Test",
    varnames = c("x", "y"),
    expectation = c(1, 2),
    covariance = valid_cov_2x2
  )
  
  expect_true(isS4(obj))
  expect_s4_class(obj, "bl")
})

test_that("bl constructor creates valid objects", {
  # Create a simple bl object
  obj <- bl(
    name = "Test",
    varnames = c("x", "y"),
    expectation = c(1, 2),
    covariance = valid_cov_2x2
  )
  
  expect_s4_class(obj, "bl")
  expect_equal(obj@name, "Test")
  expect_equal(obj@varnames, c("x", "y"))
  expect_equal(obj@expectation, c(1, 2))
  expect_equal(obj@covariance, valid_cov_2x2)
})

test_that("bl constructor works with scalar covariance", {
  obj <- bl(
    name = "Scalar",
    varnames = "x",
    expectation = 5,
    covariance = 2.5
  )
  
  expect_s4_class(obj, "bl")
  expect_equal(obj@name, "Scalar")
  expect_equal(obj@varnames, "x")
  expect_equal(obj@expectation, 5)
  expect_equal(obj@covariance, 2.5)
})

test_that("bl constructor works with 1x1 matrix covariance", {
  obj <- bl(
    name = "OneByOne",
    varnames = "x",
    expectation = 3,
    covariance = matrix(4, 1, 1)
  )
  
  expect_s4_class(obj, "bl")
  expect_equal(obj@covariance, matrix(4, 1, 1))
})

# --- Tests for bl Validity Checks ---

test_that("bl validity: name must have length 1", {
  expect_error(
    bl(
      name = c("Test1", "Test2"),
      varnames = "x",
      expectation = 0,
      covariance = 1
    ),
    regexp = "Slot 'name' must have length 1, not 2"
  )
})

test_that("bl validity: variable names must be unique", {
  expect_error(
    bl(
      name = "Test",
      varnames = c("x", "y", "x"),
      expectation = c(1, 2, 3),
      covariance = valid_cov_3x3
    ),
    regexp = "All variable names in 'varnames' must be unique"
  )
})

test_that("bl validity: expectation length must match variables", {
  expect_error(
    bl(
      name = "Test",
      varnames = c("x", "y"),
      expectation = c(1, 2, 3),
      covariance = valid_cov_2x2
    ),
    regexp = "Slot 'expectation' has length 3.*should match.*2"
  )
})

test_that("bl validity: expectation cannot contain NA", {
  expect_error(
    bl(
      name = "Test",
      varnames = c("x", "y"),
      expectation = c(1, NA),
      covariance = valid_cov_2x2
    ),
    regexp = "The 'expectation' vector must not contain NA values"
  )
})

test_that("bl validity: covariance cannot contain NA", {
  cov_with_na <- matrix(c(1, 0.5, NA, 1), 2, 2)
  expect_error(
    bl(
      name = "Test",
      varnames = c("x", "y"),
      expectation = c(1, 2),
      covariance = cov_with_na
    ),
    regexp = "The 'covariance' matrix or value must not contain NA values"
  )
})

test_that("bl validity: covariance matrix dimensions must match variables", {
  expect_error(
    bl(
      name = "Test",
      varnames = c("x", "y", "z"),
      expectation = c(1, 2, 3),
      covariance = valid_cov_2x2
    ),
    regexp = "The covariance matrix has dimensions.*2.*2.*must be.*3.*3"
  )
})

test_that("bl validity: covariance matrix must be symmetric", {
  asymmetric_cov <- matrix(c(1, 0.5, 0.3, 1), 2, 2)
  expect_error(
    bl(
      name = "Test",
      varnames = c("x", "y"),
      expectation = c(1, 2),
      covariance = asymmetric_cov
    ),
    regexp = "The covariance matrix is not symmetric"
  )
})

test_that("bl validity: covariance matrix must be positive semi-definite", {
  non_psd_cov <- matrix(c(1, 2, 2, 1), 2, 2)
  expect_error(
    bl(
      name = "Test",
      varnames = c("x", "y"),
      expectation = c(1, 2),
      covariance = non_psd_cov
    ),
    regexp = "The covariance matrix is not positive semi-definite"
  )
})

test_that("bl validity: scalar covariance must be non-negative", {
  expect_error(
    bl(
      name = "Test",
      varnames = "x",
      expectation = 1,
      covariance = -1
    ),
    regexp = "The scalar covariance.*must be non-negative"
  )
})

test_that("bl validity: scalar covariance for single variable only", {
  expect_error(
    bl(
      name = "Test",
      varnames = "x",
      expectation = 1,
      covariance = c(1, 2)
    ),
    regexp = "For a single variable, covariance should be a single numeric value"
  )
})

# --- Tests for print.bl Method ---

test_that("print.bl displays object correctly", {
  obj <- bl(
    name = "Test Print",
    varnames = c("x", "y"),
    expectation = c(1.12345, 2.98765),
    covariance = valid_cov_2x2
  )
  
  # Capture output
  output <- capture.output(print(obj))
  
  # Check that output contains key elements
  expect_true(any(grepl("Test Print", output)))
  expect_true(any(grepl("Expectation", output)))
  expect_true(any(grepl("Covariance", output)))
})

test_that("print.bl respects digits parameter", {
  obj <- bl(
    name = "Digits Test",
    varnames = c("x", "y"),
    expectation = c(1.123456789, 2.987654321),
    covariance = matrix(c(1.555, 0.5, 0.5, 1.111), 2, 2)
  )
  
  # Capture output with different digits
  output2 <- capture.output(print(obj, digits = 2))
  output4 <- capture.output(print(obj, digits = 4))
  
  # Check that outputs are different (due to rounding)
  expect_false(identical(output2, output4))
})

test_that("print.bl returns object invisibly", {
  obj <- bl(
    name = "Return Test",
    varnames = "x",
    expectation = 1,
    covariance = 1
  )
  
  result <- withVisible(print(obj))
  expect_false(result$visible)
  expect_equal(result$value, obj)
})

test_that("print.bl handles scalar covariance", {
  obj <- bl(
    name = "Scalar Test",
    varnames = "x",
    expectation = 5,
    covariance = 2.5
  )
  
  output <- capture.output(print(obj))
  expect_true(any(grepl("Scalar Test", output)))
  expect_true(any(grepl("2.5", output)))
})

# --- Tests for show.bl Method ---

test_that("show.bl displays object correctly", {
  obj <- bl(
    name = "Show Test",
    varnames = c("x", "y"),
    expectation = c(1, 2),
    covariance = valid_cov_2x2
  )
  
  # Capture output from show
  output <- capture.output(show(obj))
  
  # Check that output contains key elements
  expect_true(any(grepl("Show Test", output)))
  expect_true(any(grepl("Expectation", output)))
})

test_that("show.bl calls print internally", {
  obj <- bl(
    name = "Internal Test",
    varnames = "x",
    expectation = 1,
    covariance = 1
  )
  
  # Both should produce similar output
  show_output <- capture.output(show(obj))
  print_output <- capture.output(print(obj))
  
  expect_equal(show_output, print_output)
})

# --- Tests for plot.bl Method ---

test_that("plot.bl creates plot for matrix covariance", {
  obj <- bl(
    name = "Plot Test",
    varnames = c("x", "y", "z"),
    expectation = c(1, 2, 3),
    covariance = valid_cov_3x3
  )
  
  # Test that plot executes without error
  expect_no_error(plot(obj))
})

test_that("plot.bl handles scalar covariance appropriately", {
  obj <- bl(
    name = "Scalar Plot",
    varnames = "x",
    expectation = 1,
    covariance = 1
  )
  
  # Should print message instead of plotting
  output <- capture.output(plot(obj))
  expect_true(any(grepl("No point in plotting a scalar", output)))
})

test_that("plot.bl restores graphics parameters", {
  obj <- bl(
    name = "Par Test",
    varnames = c("x", "y"),
    expectation = c(1, 2),
    covariance = valid_cov_2x2
  )
  
  # Get current par settings
  old_par <- par(no.readonly = TRUE)
  
  # Create plot
  plot(obj)
  
  # Check that some parameters are restored (margins)
  new_par <- par(no.readonly = TRUE)
  expect_equal(old_par$mar, new_par$mar)
})

# --- Edge Cases and Additional Tests ---

test_that("bl works with zero covariance", {
  obj <- bl(
    name = "Zero Var",
    varnames = "x",
    expectation = 5,
    covariance = 0
  )
  
  expect_s4_class(obj, "bl")
  expect_equal(obj@covariance, 0)
})

test_that("bl works with large number of variables", {
  n <- 10
  obj <- bl(
    name = "Large",
    varnames = paste0("v", 1:n),
    expectation = rep(0, n),
    covariance = diag(n)
  )
  
  expect_s4_class(obj, "bl")
  expect_equal(length(obj@varnames), n)
})

test_that("bl preserves variable order", {
  obj <- bl(
    name = "Order Test",
    varnames = c("z", "a", "m"),
    expectation = c(3, 1, 2),
    covariance = diag(3)
  )
  
  expect_equal(obj@varnames, c("z", "a", "m"))
  expect_equal(obj@expectation, c(3, 1, 2))
})
