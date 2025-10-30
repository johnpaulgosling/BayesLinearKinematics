# --- Helper Function ---
create_bl <- function(name, varnames, expectation, covariance) {
  bl(
    name = name, varnames = varnames,
    expectation = expectation, covariance = covariance
  )
}

# --- Test Cases ---

test_that("bl_resolution: Correct calculation - various matrix cases", {
  # Case 1: Simple diagonal matrices, positive resolution
  bl_prior1 <- create_bl("P1", c("V1", "V2"), c(0, 0), diag(c(4, 10)))
  bl_adj1 <- create_bl("A1", c("V1", "V2"), c(1, 1), diag(c(2, 5)))
  res1 <- bl_resolution(bl_prior1, bl_adj1)
  expected1 <- setNames(c(1 - 2 / 4, 1 - 5 / 10), c("V1", "V2"))
  expect_equal(res1, expected1)
  expect_named(res1, c("V1", "V2"))

  # Case 2: Non-diagonal matrices (using example from function doc)
  bl_prior2 <- create_bl(
    "Prior State", c("P1", "P2", "P3"), c(10, 20, 30),
    matrix(c(4, 1, 0.5, 1, 5, 1, 0.5, 1, 6), 3, 3)
  )
  bl_adj2 <- create_bl(
    "Adjusted State (Lower Var)", c("P1", "P2", "P3"), c(11, 20.5, 29),
    matrix(c(2, 0.5, 0.2, 0.5, 2.5, 0.5, 0.2, 0.5, 3), 3, 3)
  )
  res2 <- bl_resolution(bl_prior2, bl_adj2)
  expected2 <- setNames(c(1 - 2 / 4, 1 - 2.5 / 5, 1 - 3 / 6), c("P1", "P2", "P3"))
  expect_equal(res2, expected2)
  expect_named(res2, c("P1", "P2", "P3"))

  # Case 3: Negative resolution (variance increase)
  bl_prior3 <- create_bl("P3", c("X", "Y"), c(0, 0), diag(c(4, 5)))
  bl_adj3 <- create_bl("A3", c("X", "Y"), c(1, 1), diag(c(5, 4))) # Var(X) increased, Var(Y) decreased
  res3 <- bl_resolution(bl_prior3, bl_adj3)
  expected3 <- setNames(c(1 - 5 / 4, 1 - 4 / 5), c("X", "Y"))
  expect_equal(res3, expected3) # Should be c(-0.25, 0.2)
  expect_named(res3, c("X", "Y"))

  # Case 4: Resolution exactly 0 (no change in variance)
  bl_prior4 <- create_bl("P4", c("Z"), 0, matrix(5))
  bl_adj4 <- create_bl("A4", c("Z"), 1, matrix(5))
  res4 <- bl_resolution(bl_prior4, bl_adj4)
  expect_equal(res4, setNames(0, "Z"))
  expect_named(res4, "Z")

  # Case 5: Resolution exactly 1 (variance -> 0)
  bl_prior5 <- create_bl("P5", c("W1", "W2"), c(1, 1), diag(c(2, 3)))
  bl_adj5 <- create_bl("A5", c("W1", "W2"), c(2, 2), diag(c(0, 0))) # Adjusted variance is zero
  res5 <- bl_resolution(bl_prior5, bl_adj5)
  expected5 <- setNames(c(1 - 0 / 2, 1 - 0 / 3), c("W1", "W2"))
  expect_equal(res5, expected5)
  expect_named(res5, c("W1", "W2"))
})

test_that("bl_resolution: Correct calculation - scalar case", {
  # Case 1: Positive resolution
  bl_prior_s1 <- create_bl("Ps1", "S", 10, 4) # Scalar covariance
  bl_adj_s1 <- create_bl("As1", "S", 11, 2)
  res_s1 <- bl_resolution(bl_prior_s1, bl_adj_s1)
  expect_equal(res_s1, setNames(0.5, "S"))
  expect_named(res_s1, "S")
  expect_length(res_s1, 1)

  # Case 2: Negative resolution
  bl_prior_s2 <- create_bl("Ps2", "T", 5, 4)
  bl_adj_s2 <- create_bl("As2", "T", 6, 5) # Variance increased
  res_s2 <- bl_resolution(bl_prior_s2, bl_adj_s2)
  expect_equal(res_s2, setNames(-0.25, "T"))
  expect_named(res_s2, "T")
  expect_length(res_s2, 1)

  # Case 3: Using 1x1 matrix still works
  bl_prior_s3 <- create_bl("Ps3", "U", 10, matrix(4)) # 1x1 matrix covariance
  bl_adj_s3 <- create_bl("As3", "U", 11, matrix(2))
  res_s3 <- bl_resolution(bl_prior_s3, bl_adj_s3)
  expect_equal(res_s3, setNames(0.5, "U"))
  expect_named(res_s3, "U")
  expect_length(res_s3, 1)
})

test_that("bl_resolution: Error Handling - Incorrect Input Types", {
  bl_ok <- create_bl("OK", "A", 0, 1)
  invalid_input1 <- "not a bl object"
  invalid_input2 <- 12345
  invalid_input3 <- list(a = 1)

  expect_error(bl_resolution(invalid_input1, bl_ok),
    "Input 'x' must be an object of class 'bl'.",
    fixed = TRUE
  )
  expect_error(bl_resolution(bl_ok, invalid_input1),
    "Input 'y' must be an object of class 'bl'.",
    fixed = TRUE
  )

  expect_error(bl_resolution(invalid_input2, bl_ok),
    "Input 'x' must be an object of class 'bl'.",
    fixed = TRUE
  )
  expect_error(bl_resolution(bl_ok, invalid_input2),
    "Input 'y' must be an object of class 'bl'.",
    fixed = TRUE
  )

  expect_error(bl_resolution(invalid_input3, bl_ok),
    "Input 'x' must be an object of class 'bl'.",
    fixed = TRUE
  )
  expect_error(bl_resolution(bl_ok, invalid_input3),
    "Input 'y' must be an object of class 'bl'.",
    fixed = TRUE
  )
})

test_that("bl_resolution: Error Handling - Variable Mismatches", {
  bl_prior <- create_bl("P", c("A", "B"), c(0, 0), diag(2))
  bl_adj_ok <- create_bl("A", c("A", "B"), c(1, 1), diag(c(0.5, 0.5)))

  # Different number of variables
  bl_adj_diff_len <- create_bl("A_diff_len", c("A"), 1, 0.5)
  expect_error(bl_resolution(bl_prior, bl_adj_diff_len),
    "Objects contain different numbers of variables",
    fixed = TRUE
  ) # Partial message check

  # Different variable names
  bl_adj_diff_names <- create_bl("A_diff_names", c("A", "C"), c(1, 1), diag(c(0.5, 0.5)))
  expect_error(bl_resolution(bl_prior, bl_adj_diff_names),
    "Variable names or their order do not match between objects.",
    fixed = TRUE
  )

  # Same names, different order
  bl_adj_diff_order <- create_bl("A_diff_order", c("B", "A"), c(1, 1), matrix(c(0.5, 0.1, 0.1, 0.5), 2, 2))
  expect_error(bl_resolution(bl_prior, bl_adj_diff_order),
    "Variable names or their order do not match between objects.",
    fixed = TRUE
  )
})

test_that("bl_resolution: Error Handling - Invalid Covariance Structure", {
  bl_prior_multi_var <- create_bl("P", c("A", "B"), c(0, 0), diag(2))
  bl_adj_multi_var <- create_bl("A", c("A", "B"), c(1, 1), diag(c(0.5, 0.5)))
  bl_prior_single_var <- create_bl("P", "A", 0, 2)
  bl_adj_single_var <- create_bl("A", "A", 1, 1)

  # Multi-var names but scalar covariance
  bl_prior_scalar_cov <- bl_prior_multi_var
  bl_prior_scalar_cov@covariance <- 1 # Incorrect structure
  expect_error(bl_resolution(bl_prior_scalar_cov, bl_adj_multi_var),
    "Object 'x' has >1 variable name but covariance is not a matrix.",
    fixed = TRUE
  )

  # Single var name but matrix covariance (not 1x1)
  bl_adj_matrix_cov <- bl_adj_single_var
  bl_adj_matrix_cov@covariance <- matrix(c(1, 0, 0, 1), 2, 2) # Incorrect structure
  expect_error(bl_resolution(bl_prior_single_var, bl_adj_matrix_cov),
    "Object 'y' has 1 variable name but covariance is not scalar or 1x1 matrix.",
    fixed = TRUE
  )
})


test_that("bl_resolution: Error Handling - Non-positive Prior Variance", {
  bl_adj_ok <- create_bl("A", c("A", "B"), c(1, 1), diag(c(0.5, 0.5)))

  # Scalar zero/negative case
  bl_prior_scalar_zero <- create_bl("Ps_zero", "Z", 0, 0)
  bl_adj_scalar_ok <- create_bl("As_ok", "Z", 1, 0.5)
  expect_error(bl_resolution(bl_prior_scalar_zero, bl_adj_scalar_ok),
    "Resolution calculation failed: Prior variance is zero or negative for variable(s): Z",
    fixed = TRUE
  )

  bl_prior_scalar_neg <- bl_prior_scalar_zero
  slot(bl_prior_scalar_neg, "covariance", check = FALSE) <- -1
  expect_error(bl_resolution(bl_prior_scalar_neg, bl_adj_scalar_ok),
    "Resolution calculation failed: Prior variance is zero or negative for variable(s): Z",
    fixed = TRUE
  )
})

test_that("bl_resolution: Output properties", {
  bl_prior <- create_bl("P", c("A", "B", "C"), c(0, 0, 0), diag(c(2, 4, 6)))
  bl_adj <- create_bl("A", c("A", "B", "C"), c(1, 1, 1), diag(c(1, 2, 3)))
  result <- bl_resolution(bl_prior, bl_adj)

  expect_vector(result, ptype = double()) # Check type is numeric (double)
  expect_length(result, 3) # Check length matches number of vars
  expect_named(result, c("A", "B", "C"), ignore.order = FALSE) # Check names and order
})

# --- Additional Edge Case Tests ---

test_that("bl_resolution: Large number of variables", {
  n <- 10
  bl_prior <- create_bl(
    "Prior", paste0("v", 1:n),
    rep(0, n), diag(rep(4, n))
  )
  bl_adj <- create_bl(
    "Adjusted", paste0("v", 1:n),
    rep(1, n), diag(rep(2, n))
  )
  
  result <- bl_resolution(bl_prior, bl_adj)
  
  expect_length(result, n)
  expect_true(all(result == 0.5))
})

test_that("bl_resolution: Mixed positive and negative resolutions", {
  bl_prior <- create_bl(
    "P", c("A", "B", "C"),
    c(0, 0, 0),
    diag(c(4, 5, 6))
  )
  bl_adj <- create_bl(
    "A", c("A", "B", "C"),
    c(1, 1, 1),
    diag(c(2, 5, 8))  # A reduced, B same, C increased
  )
  
  result <- bl_resolution(bl_prior, bl_adj)
  
  expect_true(result["A"] > 0)  # Variance reduced
  expect_equal(result["B"], 0)  # No change
  expect_true(result["C"] < 0)  # Variance increased
})

test_that("bl_resolution: Perfect resolution (variance to zero)", {
  bl_prior <- create_bl("P", c("X", "Y"), c(0, 0), diag(c(2, 3)))
  bl_adj <- create_bl("A", c("X", "Y"), c(1, 1), diag(c(0, 0)))
  
  result <- bl_resolution(bl_prior, bl_adj)
  
  expect_equal(result, setNames(c(1, 1), c("X", "Y")))
})

test_that("bl_resolution: Small variance changes", {
  bl_prior <- create_bl("P", "X", 0, 1.0)
  bl_adj <- create_bl("A", "X", 0.1, 0.99)
  
  result <- bl_resolution(bl_prior, bl_adj)
  
  expect_true(result["X"] > 0)
  expect_true(result["X"] < 0.1)
})

test_that("bl_resolution: Very small variances", {
  bl_prior <- create_bl("P", "X", 0, 1e-6)
  bl_adj <- create_bl("A", "X", 0, 1e-7)
  
  result <- bl_resolution(bl_prior, bl_adj)
  
  expect_true(result["X"] > 0)
  expect_true(result["X"] < 1)
})

test_that("bl_resolution: Correlated variables", {
  cov_prior <- matrix(c(4, 2, 2, 5), 2, 2)
  cov_adj <- matrix(c(2, 1, 1, 2.5), 2, 2)
  
  bl_prior <- create_bl("P", c("A", "B"), c(0, 0), cov_prior)
  bl_adj <- create_bl("A", c("A", "B"), c(1, 1), cov_adj)
  
  result <- bl_resolution(bl_prior, bl_adj)
  
  # Check only diagonal variances matter for resolution
  expect_equal(result["A"], 1 - 2/4)
  expect_equal(result["B"], 1 - 2.5/5)
})

test_that("bl_resolution: Same objects give zero resolution", {
  bl_same <- create_bl("Same", c("X", "Y"), c(1, 2), diag(c(3, 4)))
  
  result <- bl_resolution(bl_same, bl_same)
  
  expect_equal(result, setNames(c(0, 0), c("X", "Y")))
})

test_that("bl_resolution: Works with 1x1 matrix covariance", {
  bl_prior_mat <- create_bl("P", "X", 5, matrix(4, 1, 1))
  bl_adj_mat <- create_bl("A", "X", 6, matrix(2, 1, 1))
  
  result <- bl_resolution(bl_prior_mat, bl_adj_mat)
  
  expect_equal(result, setNames(0.5, "X"))
})

test_that("bl_resolution: Handles very large resolutions", {
  bl_prior <- create_bl("P", "X", 0, 100)
  bl_adj <- create_bl("A", "X", 0.1, 0.1)
  
  result <- bl_resolution(bl_prior, bl_adj)
  
  expect_true(result["X"] > 0.99)
  expect_true(result["X"] <= 1)
})
