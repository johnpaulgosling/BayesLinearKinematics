#' Bayes linear kinematic adjustment
#'
#' @param x bl object to be adjusted.
#' @param y bl or bl_data object to adjust by.
#'
#' @return Adjusted bl object.
#' @export
#' @examples
#' bl1 <- bl(
#'   name = "Variables",
#'   varnames = c("x", "y", "z"),
#'   expectation = c(1, 2, 3),
#'   covariance = matrix(c(
#'     1, 0.5, 0.5,
#'     0.5, 1, 0.5,
#'     0.5, 0.5, 1
#'   ), 3, 3)
#' )
#'
#' bl2 <- bl_data(
#'   name = "Data",
#'   varnames = c("x", "y"),
#'   values = c(1.1, 2.1)
#' )
#'
#' bl_adjust(bl1, bl2)
bl_adjust <- function(x,
                      y) {
  # Somewhere to store error messages
  errors <- character()

  # Check x and y are BL objects
  if (class(x)[1] != "bl") {
    msg <- paste0(
      "The class of x is ",
      class(x)[1],
      ".  It should be bl."
    )
    errors <- c(errors, msg)
  }
  if (class(y)[1] != "bl" && class(y)[1] != "bl_data") {
    msg <- paste0(
      "The class of y is ",
      class(y)[1],
      ".  It should be bl or bl_data."
    )
    errors <- c(errors, msg)
  }

  # Return any errors
  if (length(errors) > 0) {
    stop(paste(
      errors,
      "\n  "
    ))
  }

  # Check that all the y variables are in x
  if (!(all(y@varnames %in% x@varnames))) {
    msg <- paste0("The variables to be adjusted are not in x.")
    errors <- c(errors, msg)
  }

  # Return any errors
  if (length(errors) > 0) {
    stop(paste(
      errors,
      "\n  "
    ))
  }

  # Simple Bayes linear update if y is of class bl_data
  if (class(y)[1] == "bl_data") {
    # Pick out mean and covariance from x that corresponds with y
    x_indices <- sapply(
      y@varnames,
      function(x_names) which(x@varnames %in% x_names)
    )
    y_expectation <- x@expectation[x_indices]
    
    # Handle scalar vs matrix covariance
    if (is.matrix(x@covariance)) {
      y_variance <- x@covariance[x_indices, x_indices, drop = FALSE]
      xy_covariance <- x@covariance[, x_indices, drop = FALSE]
    } else {
      # Scalar covariance case (single variable)
      y_variance <- x@covariance
      xy_covariance <- x@covariance
    }

    # Invert prior variance for y (using generalised inverse from MASS)
    inv_y_variance <- ginv(as.matrix(y_variance))

    # Adjusted expectation
    adj_expectation <- x@expectation +
      as.matrix(xy_covariance) %*% inv_y_variance %*% (y@values - y_expectation)

    # Adjusted variance
    adj_variance <- as.matrix(x@covariance) -
      as.matrix(xy_covariance) %*% inv_y_variance %*% t(as.matrix(xy_covariance))

    # Set up new bl object with adjusted mean and variance
    x_adj_y <- bl(
      name = paste0(
        x@name,
        "_adj_",
        y@name
      ),
      varnames = x@varnames,
      expectation = as.numeric(adj_expectation),
      covariance = (t(adj_variance) + adj_variance) / 2
    ) # Forcing symmetry
  }

  # Bayes linear kinematics update if y is of class bl
  if (class(y)[1] == "bl") {
    # Pick out mean and covariance from x that corresponds with y
    x_indices <- which(x@varnames %in% y@varnames)
    y_expectation <- x@expectation[x_indices]
    
    # Handle scalar vs matrix covariance
    if (is.matrix(x@covariance)) {
      y_variance <- x@covariance[x_indices, x_indices, drop = FALSE]
      xy_covariance <- x@covariance[, x_indices, drop = FALSE]
    } else {
      # Scalar covariance case (single variable)
      y_variance <- x@covariance
      xy_covariance <- x@covariance
    }

    # Invert prior variance for y (using generalised inverse from MASS)
    inv_y_variance <- ginv(as.matrix(y_variance))

    # Create vector multiplier
    cov_var_mult <- as.matrix(xy_covariance) %*% inv_y_variance

    # Adjusted expectation
    adj_expectation <- x@expectation +
      cov_var_mult %*% (y@expectation - y_expectation)

    # Adjusted variance
    adj_variance <- as.matrix(x@covariance) - cov_var_mult %*% t(as.matrix(xy_covariance)) +
      cov_var_mult %*% as.matrix(y@covariance) %*% t(cov_var_mult)

    # Set up new bl object with adjusted mean and variance
    x_adj_y <- bl(
      name = paste0(
        x@name,
        "_adj_",
        y@name
      ),
      varnames = x@varnames,
      expectation = as.numeric(adj_expectation),
      covariance = (t(adj_variance) + adj_variance) / 2
    ) # Forcing symmetry
  }

  return(x_adj_y)
}
