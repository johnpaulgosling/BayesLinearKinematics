set.seed(233)

cov_help <- matrix(runif(9), 3, 3)
diag(cov_help) <- rep(1, 3)
obj <- bl(
  name = "All",
  varnames = c("A", "B", "F"),
  expectation = c(0, 1, 6),
  covariance = cov_help %*% t(cov_help)
)

sub_obj <- bl_subset(obj, c("F"))

test_that("bl_subset operation extracting one", {
  expect_equal(sub_obj@name, "All_extract")
  expect_equal(sub_obj@expectation, 6)
  expect_equal(round(sub_obj@covariance, 1), matrix(2.1, 1, 1))
})

sub_obj <- bl_subset(obj, c("B", "A"))

test_that("bl_subset operation extracting two", {
  expect_equal(sub_obj@name, "All_extract")
  expect_equal(sub_obj@expectation, c(1, 0))
  expect_equal(round(sub_obj@covariance, 2), matrix(
    c(
      1.69, 1.43,
      1.43, 1.88
    ),
    byrow = TRUE, 2, 2
  ))
})

data_obj <- bl_data(
  name = "My_data",
  varnames = c("A", "B", "F"),
  values = c(1, 2, 6)
)

sub_data_obj <- bl_subset(data_obj, c("B"))

test_that("bl_subset operation extracting two", {
  expect_equal(sub_data_obj@name, "My_data_extract")
  expect_equal(sub_data_obj@values, 2)
})

cov_help <- matrix(runif(9), 3, 3)
diag(cov_help) <- rep(1, 3)
obj_2 <- bl(
  name = "Second",
  varnames = c("F", "A", "B"),
  expectation = c(0, 1, 6),
  covariance = cov_help %*% t(cov_help)
)

sub_obj_2 <- bl_subset(
  obj_2,
  obj@varnames
)

test_that("bl_subset inheriting order from other bl", {
  expect_equal(sub_obj_2@name, "Second_extract")
  expect_equal(sub_obj_2@varnames, obj@varnames)
  expect_equal(sub_obj_2@expectation, c(1, 6, 0))
  expect_equal(round(sub_obj_2@covariance, 1), matrix(
    c(
      2.4, 2.3, 1.4,
      2.3, 2.4, 1.4,
      1.4, 1.4, 1.2
    ),
    byrow = TRUE, 3, 3
  ))
})

# --- Error Handling Tests ---

test_that("bl_subset: Error on incorrect input type", {
  invalid_input <- "not a bl object"
  
  expect_error(
    bl_subset(invalid_input, c("A")),
    regexp = "The class of x is.*should be bl or bl_data"
  )
  
  expect_error(
    bl_subset(123, c("A")),
    regexp = "The class of x is.*should be bl or bl_data"
  )
})

test_that("bl_subset: Error on missing variables", {
  obj_test <- bl(
    name = "Test",
    varnames = c("A", "B", "C"),
    expectation = c(1, 2, 3),
    covariance = diag(3)
  )
  
  expect_error(
    bl_subset(obj_test, c("D")),
    regexp = "The variables to be extracted are not in x"
  )
  
  expect_error(
    bl_subset(obj_test, c("A", "D")),
    regexp = "The variables to be extracted are not in x"
  )
})

# --- Additional Tests ---

test_that("bl_subset: Extract single variable from bl", {
  obj_test <- bl(
    name = "Multi",
    varnames = c("X", "Y", "Z"),
    expectation = c(10, 20, 30),
    covariance = matrix(c(1, 0.5, 0.2, 0.5, 2, 0.3, 0.2, 0.3, 3), 3, 3)
  )
  
  result <- bl_subset(obj_test, "Y")
  
  expect_s4_class(result, "bl")
  expect_equal(result@varnames, "Y")
  expect_equal(result@expectation, 20)
  expect_equal(dim(result@covariance), c(1, 1))
  expect_equal(as.numeric(result@covariance), 2)
})

test_that("bl_subset: Extract all variables returns equivalent object", {
  obj_test <- bl(
    name = "Full",
    varnames = c("A", "B"),
    expectation = c(1, 2),
    covariance = matrix(c(1, 0.5, 0.5, 1), 2, 2)
  )
  
  result <- bl_subset(obj_test, c("A", "B"))
  
  expect_equal(result@expectation, obj_test@expectation)
  expect_equal(result@covariance, obj_test@covariance)
  expect_equal(result@varnames, obj_test@varnames)
})

test_that("bl_subset: Permutation order is preserved", {
  obj_test <- bl(
    name = "Permute",
    varnames = c("A", "B", "C", "D"),
    expectation = c(1, 2, 3, 4),
    covariance = diag(4)
  )
  
  # Extract in different order
  result <- bl_subset(obj_test, c("D", "B", "A"))
  
  expect_equal(result@varnames, c("D", "B", "A"))
  expect_equal(result@expectation, c(4, 2, 1))
  expect_equal(diag(result@covariance), c(1, 1, 1))
})

test_that("bl_subset: Extract single variable from bl_data", {
  data_test <- bl_data(
    name = "Data",
    varnames = c("X", "Y", "Z"),
    values = c(10, 20, 30)
  )
  
  result <- bl_subset(data_test, "Y")
  
  expect_s4_class(result, "bl_data")
  expect_equal(result@varnames, "Y")
  expect_equal(result@values, 20)
})

test_that("bl_subset: Extract multiple variables from bl_data", {
  data_test <- bl_data(
    name = "Multi Data",
    varnames = c("A", "B", "C", "D"),
    values = c(1, 2, 3, 4)
  )
  
  result <- bl_subset(data_test, c("C", "A"))
  
  expect_s4_class(result, "bl_data")
  expect_equal(result@varnames, c("C", "A"))
  expect_equal(result@values, c(3, 1))
})

test_that("bl_subset: Preserves covariance structure", {
  obj_test <- bl(
    name = "Cov Test",
    varnames = c("A", "B", "C"),
    expectation = c(0, 0, 0),
    covariance = matrix(c(
      1, 0.5, 0.3,
      0.5, 2, 0.4,
      0.3, 0.4, 3
    ), 3, 3)
  )
  
  result <- bl_subset(obj_test, c("A", "C"))
  
  # Check that the covariance submatrix is correct
  expected_cov <- matrix(c(1, 0.3, 0.3, 3), 2, 2)
  expect_equal(result@covariance, expected_cov)
  
  # Check symmetry is preserved
  expect_true(isSymmetric(result@covariance))
})

test_that("bl_subset: Name includes _extract suffix", {
  obj_test <- bl(
    name = "Original",
    varnames = c("A", "B"),
    expectation = c(1, 2),
    covariance = diag(2)
  )
  
  result <- bl_subset(obj_test, "A")
  
  expect_equal(result@name, "Original_extract")
})

test_that("bl_subset: Works with duplicate extraction request", {
  obj_test <- bl(
    name = "Test",
    varnames = c("A", "B", "C"),
    expectation = c(1, 2, 3),
    covariance = diag(3)
  )
  
  result1 <- bl_subset(obj_test, "A")
  result2 <- bl_subset(obj_test, "A")
  
  expect_equal(result1@expectation, result2@expectation)
  expect_equal(result1@covariance, result2@covariance)
})
