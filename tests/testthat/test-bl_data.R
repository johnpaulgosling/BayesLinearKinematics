# Tests for bl_data class and its methods

# --- Tests for bl_data Constructor ---

test_that("bl_data creates S4 class object", {
  obj <- bl_data(
    name = "S4 Test",
    varnames = c("x", "y"),
    values = c(1, 2)
  )
  
  expect_true(isS4(obj))
  expect_s4_class(obj, "bl_data")
})

test_that("bl_data constructor creates valid objects", {
  obj <- bl_data(
    name = "Test Data",
    varnames = c("x", "y", "z"),
    values = c(1.5, 2.3, 3.7)
  )
  
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@name, "Test Data")
  expect_equal(obj@varnames, c("x", "y", "z"))
  expect_equal(obj@values, c(1.5, 2.3, 3.7))
})

test_that("bl_data constructor works with single variable", {
  obj <- bl_data(
    name = "Single",
    varnames = "x",
    values = 42
  )
  
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@varnames, "x")
  expect_equal(obj@values, 42)
})

test_that("bl_data constructor works with zero values", {
  obj <- bl_data(
    name = "Zeros",
    varnames = c("a", "b"),
    values = c(0, 0)
  )
  
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@values, c(0, 0))
})

test_that("bl_data constructor works with negative values", {
  obj <- bl_data(
    name = "Negatives",
    varnames = c("x", "y"),
    values = c(-5, -10.5)
  )
  
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@values, c(-5, -10.5))
})

# --- Tests for bl_data Validity Checks ---

test_that("bl_data validity: name must have length 1", {
  expect_error(
    bl_data(
      name = c("Data1", "Data2"),
      varnames = "x",
      values = 1
    ),
    regexp = "Slot 'name' must have length 1, not 2"
  )
})

test_that("bl_data validity: variable names must be unique", {
  expect_error(
    bl_data(
      name = "Test",
      varnames = c("x", "y", "x"),
      values = c(1, 2, 3)
    ),
    regexp = "All variable names in varnames must be unique"
  )
})

test_that("bl_data validity: values length must match variables", {
  expect_error(
    bl_data(
      name = "Test",
      varnames = c("x", "y"),
      values = c(1, 2, 3)
    ),
    regexp = "Slot 'values' has length 3.*should match.*2"
  )
  
  expect_error(
    bl_data(
      name = "Test",
      varnames = c("x", "y", "z"),
      values = c(1, 2)
    ),
    regexp = "Slot 'values' has length 2.*should match.*3"
  )
})

test_that("bl_data validity: values must be finite", {
  # Test with NA
  expect_error(
    bl_data(
      name = "Test",
      varnames = c("x", "y"),
      values = c(1, NA)
    ),
    regexp = "All 'values' must be finite.*not NA, Inf, or -Inf"
  )
  
  # Test with Inf
  expect_error(
    bl_data(
      name = "Test",
      varnames = c("x", "y"),
      values = c(1, Inf)
    ),
    regexp = "All 'values' must be finite.*not NA, Inf, or -Inf"
  )
  
  # Test with -Inf
  expect_error(
    bl_data(
      name = "Test",
      varnames = c("x", "y"),
      values = c(-Inf, 2)
    ),
    regexp = "All 'values' must be finite.*not NA, Inf, or -Inf"
  )
})

test_that("bl_data validity: error messages include problem indices", {
  expect_error(
    bl_data(
      name = "Test",
      varnames = c("a", "b", "c", "d"),
      values = c(1, NA, 3, Inf)
    ),
    regexp = "Problem found at indices: 2, 4"
  )
})

# --- Tests for print.bl_data Method ---

test_that("print.bl_data displays object correctly", {
  obj <- bl_data(
    name = "Print Test",
    varnames = c("x", "y"),
    values = c(1.123456, 2.987654)
  )
  
  # Capture output
  output <- capture.output(print(obj))
  
  # Check that output contains key elements
  expect_true(any(grepl("Print Test", output)))
  expect_true(any(grepl("Observed values", output)))
})

test_that("print.bl_data respects digits parameter", {
  obj <- bl_data(
    name = "Digits Test",
    varnames = c("x", "y"),
    values = c(1.123456789, 2.987654321)
  )
  
  # Capture output with different digits
  output_default <- capture.output(print(obj))
  output2 <- capture.output(print(obj, digits = 2))
  output6 <- capture.output(print(obj, digits = 6))
  
  # Check that outputs are different due to rounding
  expect_false(identical(output2, output6))
})

test_that("print.bl_data returns object invisibly", {
  obj <- bl_data(
    name = "Return Test",
    varnames = "x",
    values = 1
  )
  
  result <- withVisible(print(obj))
  expect_false(result$visible)
  expect_equal(result$value, obj)
})

test_that("print.bl_data handles single value", {
  obj <- bl_data(
    name = "Single",
    varnames = "temperature",
    values = 25.5
  )
  
  output <- capture.output(print(obj))
  expect_true(any(grepl("Single", output)))
  expect_true(any(grepl("temperature", output)))
})

test_that("print.bl_data handles many values", {
  obj <- bl_data(
    name = "Many Values",
    varnames = paste0("v", 1:10),
    values = 1:10
  )
  
  output <- capture.output(print(obj))
  expect_true(any(grepl("Many Values", output)))
})

# --- Tests for show.bl_data Method ---

test_that("show.bl_data displays object correctly", {
  obj <- bl_data(
    name = "Show Test",
    varnames = c("x", "y"),
    values = c(10, 20)
  )
  
  # Capture output from show
  output <- capture.output(show(obj))
  
  # Check that output contains key elements
  expect_true(any(grepl("Show Test", output)))
  expect_true(any(grepl("Observed values", output)))
})

test_that("show.bl_data calls print internally", {
  obj <- bl_data(
    name = "Internal Test",
    varnames = "x",
    values = 5
  )
  
  # Both should produce similar output (show uses print with defaults)
  show_output <- capture.output(show(obj))
  print_output <- capture.output(print(obj))
  
  expect_equal(show_output, print_output)
})

# --- Tests for plot.bl_data Method ---

test_that("plot.bl_data creates plot", {
  obj <- bl_data(
    name = "Plot Test",
    varnames = c("a", "b", "c"),
    values = c(1, 2, 3)
  )
  
  # Test that plot executes without error
  expect_no_error(plot(obj))
})

test_that("plot.bl_data handles single value", {
  obj <- bl_data(
    name = "Single Plot",
    varnames = "x",
    values = 42
  )
  
  # Should still create a plot without error
  expect_no_error(plot(obj))
})

test_that("plot.bl_data handles many values", {
  obj <- bl_data(
    name = "Many Plot",
    varnames = paste0("v", 1:20),
    values = rnorm(20)
  )
  
  # Should create a plot without error
  expect_no_error(plot(obj))
})

test_that("plot.bl_data handles negative and zero values", {
  obj <- bl_data(
    name = "Mixed Values",
    varnames = c("neg", "zero", "pos"),
    values = c(-5, 0, 5)
  )
  
  expect_no_error(plot(obj))
})

# --- Edge Cases and Additional Tests ---

test_that("bl_data works with very small values", {
  obj <- bl_data(
    name = "Small",
    varnames = c("x", "y"),
    values = c(1e-10, 1e-15)
  )
  
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@values, c(1e-10, 1e-15))
})

test_that("bl_data works with very large values", {
  obj <- bl_data(
    name = "Large",
    varnames = c("x", "y"),
    values = c(1e10, 1e15)
  )
  
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@values, c(1e10, 1e15))
})

test_that("bl_data preserves variable order", {
  obj <- bl_data(
    name = "Order Test",
    varnames = c("z", "a", "m"),
    values = c(3, 1, 2)
  )
  
  expect_equal(obj@varnames, c("z", "a", "m"))
  expect_equal(obj@values, c(3, 1, 2))
})

test_that("bl_data works with special variable names", {
  obj <- bl_data(
    name = "Special Names",
    varnames = c("var.1", "var_2", "var-3"),
    values = c(1, 2, 3)
  )
  
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@varnames, c("var.1", "var_2", "var-3"))
})

test_that("bl_data works with numeric variable names", {
  obj <- bl_data(
    name = "Numeric Names",
    varnames = c("1", "2", "3"),
    values = c(10, 20, 30)
  )
  
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@varnames, c("1", "2", "3"))
})
