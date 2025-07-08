#' Calculate Bayes linear variance resolution
#'
#' Computes the resolution for each variable, defined as
#' `1 - Var_adjusted / Var_prior`, based on prior and adjusted beliefs.
#'
#' The resolution measures the proportion of prior variance removed by the
#' adjustment process for each variable. It should ideally range from 0 (no
#' variance reduction) to 1 (variance reduced to zero).
#'
#' @param x A 'bl' object representing the **prior** beliefs
#' (before adjustment).
#' @param y A 'bl' object representing the **adjusted** beliefs
#' (after adjustment).
#'
#' @return A named numeric vector of resolutions, ideally on the 0-1 scale.
#'   Names correspond to the variable names in the input objects. Issues a
#'   warning if any prior variances are non-positive or if calculated
#'   resolutions fall outside 0-1.
#' @export
#' @importFrom methods is # For class checking
#' @examples
#' # Prior Beliefs
#' bl_prior <- bl(
#'   name = "Prior State",
#'   varnames = c("P1", "P2", "P3"),
#'   expectation = c(10, 20, 30),
#'   covariance = matrix(c(
#'     4, 1, 0.5,
#'     1, 5, 1,
#'     0.5, 1, 6
#'   ), 3, 3)
#' )
#'
#' # Adjusted Beliefs (after some adjustment process)
#' bl_adjusted <- bl(
#'   name = "Adjusted State",
#'   varnames = c("P1", "P2", "P3"),
#'   expectation = c(11, 20.5, 29),
#'   covariance = matrix(c(
#'     2, 0.5, 0.2,
#'     0.5, 2.5, 0.5,
#'     0.2, 0.5, 3
#'   ), 3, 3)
#' )
#'
#' # Calculate resolution
#' resolutions <- bl_resolution(bl_prior, bl_adjusted)
#' print(resolutions)
#' # Should output something like:
#' # P1  P2  P3
#' # 0.5 0.5 0.5
#'
#' # Example with potential issues (e.g., zero prior variance - may warn/error)
#' bl_prior_zero_var <- bl(
#'   name = "Prior with Zero Var",
#'   varnames = "Z", expectation = 0, covariance = 0
#' )
#' bl_adjusted_zero_var <- bl(
#'   name = "Adjusted with Zero Var",
#'   varnames = "Z", expectation = 1, covariance = 0.1
#' )
#' # try(bl_resolution(bl_prior_zero_var, bl_adjusted_zero_var))
#'
bl_resolution <- function(x, y) {
  errors <- character()
  TOLERANCE <- 1e-12 # Tolerance for checking zero variance

  # --- 1. Input Validation ---

  # Check classes
  if (!is(x, "bl")) {
    errors <- c(errors, "Input 'x' must be an object of class 'bl'.")
  }
  if (!is(y, "bl")) {
    errors <- c(errors, "Input 'y' must be an object of class 'bl'.")
  }

  # Stop early if class checks failed
  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"))
  }

  # Check variable counts match
  n_vars_x <- length(x@varnames)
  n_vars_y <- length(y@varnames)
  if (n_vars_x != n_vars_y) {
    errors <- c(errors, paste0(
      "Objects contain different numbers of variables (x:",
      n_vars_x, ", y:", n_vars_y, ")."
    ))
  } else if (n_vars_x == 0) {
    errors <- c(errors, "Input objects contain no variables.")
  } else {
    # If counts match and > 0, check names and order match
    if (any(x@varnames != y@varnames)) {
      errors <- c(errors,
                  "Variable names or their order do not match between objects.")
    }
  }

  # Check covariance types compatibility
  is_scalar_x <- !is.matrix(x@covariance) && is.numeric(x@covariance) &&
    length(x@covariance) == 1
  is_scalar_y <- !is.matrix(y@covariance) && is.numeric(y@covariance) &&
    length(y@covariance) == 1

  if (n_vars_x == 1) {
    if (!is_scalar_x && !identical(dim(x@covariance), c(1L, 1L))) {
      errors <- c(errors,
                  paste0("Object 'x' has 1 variable name but covariance is ",
                         "not scalar or 1x1 matrix."))
    }
    if (!is_scalar_y && !identical(dim(y@covariance), c(1L, 1L))) {
      errors <- c(errors,
                  paste0("Object 'y' has 1 variable name but covariance is",
                  " not scalar or 1x1 matrix."))
    }
  } else if (n_vars_x > 1) {
    if (is_scalar_x || !is.matrix(x@covariance)) {
      errors <- c(errors, paste0("Object 'x' has >1 variable name but ",
            "covariance is not a matrix."))
    }
    if (is_scalar_y || !is.matrix(y@covariance)) {
      errors <- c(errors, paste0("Object 'y' has >1 variable name but ",
                                 "covariance is not a matrix."))
    }
  }

  # Stop if any validation errors found so far
  if (length(errors) > 0) {
    stop(paste(errors, collapse = "\n"))
  }

  # --- 2. Extract Variances and Check Prior Variance ---

  if (n_vars_x == 1) {
    prior_vars <- as.numeric(x@covariance)
    adjusted_vars <- as.numeric(y@covariance) # Use 'adjusted' internally
  } else {
    prior_vars <- diag(x@covariance)
    adjusted_vars <- diag(y@covariance) # Use 'adjusted' internally
  }

  # Check for non-positive prior variances
  if (any(prior_vars <= TOLERANCE)) {
    bad_indices <- which(prior_vars <= TOLERANCE)
    bad_vars <- x@varnames[bad_indices]
    # Stop execution as resolution is ill-defined
    stop(
      paste0("Resolution calculation failed: Prior variance is ",
             "zero or negative for variable(s): "),
      paste(bad_vars, collapse = ", "), "."
    )
  }

  # --- 3. Calculate Resolution ---

  resolutions <- 1 - adjusted_vars / prior_vars

  # --- 4. Bounds Check Removed ---
  # No longer checking if resolutions are outside [0, 1]

  # --- 5. Return Named Vector ---
  names(resolutions) <- x@varnames
  return(resolutions)
}
