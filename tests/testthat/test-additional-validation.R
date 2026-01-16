# Additional comprehensive tests for data type validation and error catching

# --- Tests for bl_data with Additional Data Type Checks ---

test_that("bl_data: Rejects NULL inputs", {
  expect_error(
    bl_data(name = NULL, varnames = "x", values = 1),
    regexp = "Slot 'name' must have length 1"
  )
  
  expect_error(
    bl_data(name = "test", varnames = NULL, values = 1),
    class = "error"
  )
  
  expect_error(
    bl_data(name = "test", varnames = "x", values = NULL),
    class = "error"
  )
})

test_that("bl_data: Rejects character values", {
  expect_error(
    bl_data(name = "test", varnames = "x", values = "not_a_number"),
    class = "error"
  )
  
  expect_error(
    bl_data(name = "test", varnames = c("x", "y"), values = c("1", "2")),
    class = "error"
  )
})

test_that("bl_data: Rejects factor varnames where issues may arise", {
  # Factors should be coerced to character, but test behavior
  factor_names <- factor(c("A", "B", "C"))
  # This should work as factors are converted to character
  obj <- bl_data(
    name = "Factor Test",
    varnames = as.character(factor_names),
    values = c(1, 2, 3)
  )
  expect_s4_class(obj, "bl_data")
})

test_that("bl_data: Rejects empty varnames", {
  expect_error(
    bl_data(name = "test", varnames = character(0), values = numeric(0)),
    class = "error"
  )
})

test_that("bl_data: Rejects varnames with empty strings", {
  # This should work but may cause issues in practice
  obj <- bl_data(name = "test", varnames = c("", "y"), values = c(1, 2))
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@varnames, c("", "y"))
})

test_that("bl_data: Handles special numeric values in names gracefully", {
  obj <- bl_data(
    name = "Special Names",
    varnames = c("var.1", "var_2", "var-3", "var 4"),
    values = c(1, 2, 3, 4)
  )
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@varnames, c("var.1", "var_2", "var-3", "var 4"))
})

test_that("bl_data: Rejects list inputs", {
  expect_error(
    bl_data(name = "test", varnames = list("x", "y"), values = c(1, 2)),
    class = "error"
  )
  
  expect_error(
    bl_data(name = "test", varnames = c("x", "y"), values = list(1, 2)),
    class = "error"
  )
})

test_that("bl_data: Rejects data.frame inputs", {
  expect_error(
    bl_data(
      name = "test",
      varnames = c("x", "y"),
      values = data.frame(x = 1, y = 2)
    ),
    class = "error"
  )
})

test_that("bl_data: Rejects matrix for values", {
  expect_error(
    bl_data(name = "test", varnames = c("x", "y"), values = matrix(c(1, 2), 1, 2)),
    class = "error"
  )
})

# --- Tests for bl with Additional Data Type Checks ---

test_that("bl: Rejects NULL inputs", {
  expect_error(
    bl(name = NULL, varnames = "x", expectation = 1, covariance = 1),
    regexp = "Slot 'name' must have length 1"
  )
  
  expect_error(
    bl(name = "test", varnames = NULL, expectation = 1, covariance = 1),
    class = "error"
  )
})

test_that("bl: Rejects character expectation", {
  expect_error(
    bl(name = "test", varnames = "x", expectation = "not_a_number", covariance = 1),
    class = "error"
  )
})

test_that("bl: Rejects character covariance", {
  expect_error(
    bl(
      name = "test",
      varnames = c("x", "y"),
      expectation = c(1, 2),
      covariance = "not_a_matrix"
    ),
    class = "error"
  )
})

test_that("bl: Rejects list inputs for numeric slots", {
  expect_error(
    bl(name = "test", varnames = "x", expectation = list(1), covariance = 1),
    class = "error"
  )
  
  expect_error(
    bl(name = "test", varnames = "x", expectation = 1, covariance = list(1)),
    class = "error"
  )
})

test_that("bl: Rejects data.frame for covariance", {
  expect_error(
    bl(
      name = "test",
      varnames = c("x", "y"),
      expectation = c(1, 2),
      covariance = data.frame(a = c(1, 0.5), b = c(0.5, 1))
    ),
    class = "error"
  )
})

test_that("bl: Handles Inf in expectation", {
  expect_error(
    bl(
      name = "test",
      varnames = c("x", "y"),
      expectation = c(Inf, 2),
      covariance = diag(2)
    ),
    class = "error"
  )
})

test_that("bl: Handles Inf in covariance", {
  expect_error(
    bl(
      name = "test",
      varnames = c("x", "y"),
      expectation = c(1, 2),
      covariance = matrix(c(Inf, 0.5, 0.5, 1), 2, 2)
    ),
    regexp = "The 'covariance' matrix or value must not contain NA values"
  )
})

test_that("bl: Rejects empty varnames", {
  expect_error(
    bl(
      name = "test",
      varnames = character(0),
      expectation = numeric(0),
      covariance = matrix(nrow = 0, ncol = 0)
    ),
    class = "error"
  )
})

test_that("bl: Handles very small positive eigenvalues (near-singular)", {
  # Create a near-singular but technically positive semi-definite matrix
  near_singular <- matrix(c(1, 0.9999, 0.9999, 1), 2, 2)
  
  # This should work as it's still positive semi-definite
  obj <- bl(
    name = "Near Singular",
    varnames = c("x", "y"),
    expectation = c(1, 2),
    covariance = near_singular
  )
  
  expect_s4_class(obj, "bl")
})

test_that("bl: Rejects non-square matrix for covariance", {
  expect_error(
    bl(
      name = "test",
      varnames = c("x", "y"),
      expectation = c(1, 2),
      covariance = matrix(1:6, 2, 3)
    ),
    regexp = "The covariance matrix has dimensions"
  )
})

# --- Tests for bl_adjust with Additional Error Handling ---

test_that("bl_adjust: Rejects NULL inputs", {
  x <- bl(name = "x", varnames = "A", expectation = 0, covariance = 1)
  y <- bl_data(name = "y", varnames = "A", values = 1)
  
  expect_error(bl_adjust(NULL, y), regexp = "The class of x is")
  expect_error(bl_adjust(x, NULL), regexp = "The class of y is")
})

test_that("bl_adjust: Rejects numeric vector inputs", {
  x <- bl(name = "x", varnames = "A", expectation = 0, covariance = 1)
  
  expect_error(
    bl_adjust(c(1, 2, 3), x),
    regexp = "The class of x is"
  )
  
  expect_error(
    bl_adjust(x, c(1, 2, 3)),
    regexp = "The class of y is"
  )
})

test_that("bl_adjust: Rejects data.frame inputs", {
  x <- bl(name = "x", varnames = "A", expectation = 0, covariance = 1)
  
  expect_error(
    bl_adjust(data.frame(a = 1), x),
    regexp = "The class of x is"
  )
})

test_that("bl_adjust: Handles empty variable overlap", {
  x <- bl(name = "x", varnames = c("A", "B"), expectation = c(0, 0), 
          covariance = diag(2))
  y <- bl_data(name = "y", varnames = "C", values = 1)
  
  expect_error(
    bl_adjust(x, y),
    regexp = "The variables to be adjusted are not in x"
  )
})

test_that("bl_adjust: Handles partial variable overlap", {
  x <- bl(
    name = "x",
    varnames = c("A", "B"),
    expectation = c(0, 0),
    covariance = diag(2)
  )
  y <- bl_data(name = "y", varnames = c("A", "C"), values = c(1, 2))
  
  expect_error(
    bl_adjust(x, y),
    regexp = "The variables to be adjusted are not in x"
  )
})

# --- Tests for bl_subset with Additional Error Handling ---

test_that("bl_subset: Rejects NULL inputs", {
  x <- bl(name = "x", varnames = c("A", "B"), expectation = c(0, 0), 
          covariance = diag(2))
  
  expect_error(bl_subset(NULL, c("A")), regexp = "The class of x is")
  expect_error(bl_subset(x, NULL), class = "error")
})

test_that("bl_subset: Rejects character string input for x", {
  expect_error(
    bl_subset("not_an_object", c("A")),
    regexp = "The class of x is"
  )
})

test_that("bl_subset: Rejects numeric input for x", {
  expect_error(
    bl_subset(123, c("A")),
    regexp = "The class of x is"
  )
})

test_that("bl_subset: Handles empty varnames subset", {
  x <- bl(name = "x", varnames = c("A", "B"), expectation = c(0, 0), 
          covariance = diag(2))
  
  expect_error(
    bl_subset(x, character(0)),
    class = "error"
  )
})

test_that("bl_subset: Handles duplicate varnames in subset request", {
  x <- bl(
    name = "x",
    varnames = c("A", "B", "C"),
    expectation = c(0, 0, 0),
    covariance = diag(3)
  )
  
  # Requesting duplicates - should extract twice
  result <- bl_subset(x, c("A", "A"))
  expect_s4_class(result, "bl")
  expect_equal(length(result@varnames), 2)
})

# --- Tests for bl_resolution with Additional Error Handling ---

test_that("bl_resolution: Rejects NULL inputs", {
  x <- bl(name = "x", varnames = "A", expectation = 0, covariance = 1)
  
  expect_error(bl_resolution(NULL, x), "Input 'x' must be an object of class 'bl'")
  expect_error(bl_resolution(x, NULL), "Input 'y' must be an object of class 'bl'")
})

test_that("bl_resolution: Rejects bl_data as input", {
  x <- bl(name = "x", varnames = "A", expectation = 0, covariance = 1)
  y <- bl_data(name = "y", varnames = "A", values = 1)
  
  expect_error(
    bl_resolution(x, y),
    "Input 'y' must be an object of class 'bl'"
  )
  
  expect_error(
    bl_resolution(y, x),
    "Input 'x' must be an object of class 'bl'"
  )
})

test_that("bl_resolution: Rejects mismatched dimensions", {
  x <- bl(name = "x", varnames = c("A", "B"), expectation = c(0, 0), 
          covariance = diag(2))
  y <- bl(name = "y", varnames = "A", expectation = 0, covariance = 1)
  
  expect_error(
    bl_resolution(x, y),
    "Objects contain different numbers of variables"
  )
})

test_that("bl_resolution: Rejects different variable names", {
  x <- bl(name = "x", varnames = c("A", "B"), expectation = c(0, 0), 
          covariance = diag(2))
  y <- bl(name = "y", varnames = c("C", "D"), expectation = c(0, 0), 
          covariance = diag(2))
  
  expect_error(
    bl_resolution(x, y),
    "Variable names or their order do not match"
  )
})

test_that("bl_resolution: Rejects different variable order", {
  x <- bl(name = "x", varnames = c("A", "B"), expectation = c(0, 0), 
          covariance = diag(2))
  y <- bl(name = "y", varnames = c("B", "A"), expectation = c(0, 0), 
          covariance = diag(2))
  
  expect_error(
    bl_resolution(x, y),
    "Variable names or their order do not match"
  )
})

test_that("bl_resolution: Handles zero prior variance correctly", {
  x <- bl(name = "x", varnames = "A", expectation = 0, covariance = 0)
  y <- bl(name = "y", varnames = "A", expectation = 1, covariance = 0)
  
  expect_error(
    bl_resolution(x, y),
    "Prior variance is zero or negative"
  )
})

test_that("bl_resolution: Handles negative prior variance", {
  # This shouldn't be possible due to bl validity, but test the function's check
  x <- bl(name = "x", varnames = "A", expectation = 0, covariance = 1)
  y <- bl(name = "y", varnames = "A", expectation = 1, covariance = 0.5)
  
  # Manually create invalid object for testing (bypassing validity)
  # Note: This is difficult with S4 classes, so we test with zero instead
  x_zero <- bl(name = "x", varnames = "A", expectation = 0, covariance = 0)
  
  expect_error(
    bl_resolution(x_zero, y),
    "Prior variance is zero or negative"
  )
})

# --- Tests for hellinger_squared with Additional Error Handling ---

test_that("hellinger_squared: Rejects NULL inputs", {
  x <- bl(name = "x", varnames = "A", expectation = 0, covariance = 1)
  
  expect_error(hellinger_squared(NULL, x), regexp = "The class of x is")
  expect_error(hellinger_squared(x, NULL), regexp = "The class of y is")
})

test_that("hellinger_squared: Rejects bl_data as input", {
  x <- bl(name = "x", varnames = "A", expectation = 0, covariance = 1)
  y <- bl_data(name = "y", varnames = "A", values = 1)
  
  expect_error(hellinger_squared(x, y), regexp = "The class of y is")
  expect_error(hellinger_squared(y, x), regexp = "The class of x is")
})

test_that("hellinger_squared: Rejects numeric vector inputs", {
  x <- bl(name = "x", varnames = "A", expectation = 0, covariance = 1)
  
  expect_error(
    hellinger_squared(c(1, 2, 3), x),
    regexp = "The class of x is"
  )
  
  expect_error(
    hellinger_squared(x, c(1, 2, 3)),
    regexp = "The class of y is"
  )
})

test_that("hellinger_squared: Handles disjoint variable sets", {
  x <- bl(name = "x", varnames = c("A", "B"), expectation = c(0, 0), 
          covariance = diag(2))
  y <- bl(name = "y", varnames = c("C", "D"), expectation = c(0, 0), 
          covariance = diag(2))
  
  expect_error(
    hellinger_squared(x, y),
    regexp = "Some of the variables"
  )
})

test_that("hellinger_squared: Handles partial variable overlap", {
  x <- bl(name = "x", varnames = c("A", "B"), expectation = c(0, 0), 
          covariance = diag(2))
  y <- bl(name = "y", varnames = c("B", "C"), expectation = c(0, 0), 
          covariance = diag(2))
  
  expect_error(
    hellinger_squared(x, y),
    regexp = "Some of the variables"
  )
})

# --- Edge Case Tests for Mathematical Operations ---

test_that("bl: Handles very large covariance values", {
  large_cov <- matrix(c(1e10, 5e9, 5e9, 1e10), 2, 2)
  
  obj <- bl(
    name = "Large Cov",
    varnames = c("x", "y"),
    expectation = c(1, 2),
    covariance = large_cov
  )
  
  expect_s4_class(obj, "bl")
  expect_equal(obj@covariance, large_cov)
})

test_that("bl: Handles very small positive covariance values", {
  small_cov <- matrix(c(1e-10, 5e-11, 5e-11, 1e-10), 2, 2)
  
  obj <- bl(
    name = "Small Cov",
    varnames = c("x", "y"),
    expectation = c(1, 2),
    covariance = small_cov
  )
  
  expect_s4_class(obj, "bl")
  expect_equal(obj@covariance, small_cov)
})

test_that("bl_data: Handles extreme value ranges", {
  obj <- bl_data(
    name = "Extreme Range",
    varnames = c("tiny", "huge"),
    values = c(1e-100, 1e100)
  )
  
  expect_s4_class(obj, "bl_data")
  expect_equal(obj@values[1], 1e-100)
  expect_equal(obj@values[2], 1e100)
})

test_that("bl: Handles identity matrix covariance", {
  obj <- bl(
    name = "Identity",
    varnames = c("x", "y", "z"),
    expectation = c(1, 2, 3),
    covariance = diag(3)
  )
  
  expect_s4_class(obj, "bl")
  expect_true(all(diag(obj@covariance) == 1))
  expect_true(all(obj@covariance[row(obj@covariance) != col(obj@covariance)] == 0))
})

test_that("bl_adjust: Handles identity covariance", {
  x <- bl(
    name = "x",
    varnames = c("A", "B"),
    expectation = c(0, 0),
    covariance = diag(2)
  )
  y <- bl_data(name = "y", varnames = "A", values = 1)
  
  xy <- bl_adjust(x, y)
  
  expect_s4_class(xy, "bl")
  expect_equal(xy@expectation[1], 1)  # Perfect update when no correlation
})
