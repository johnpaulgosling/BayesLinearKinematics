#' Union class for matrix or numeric
#' @keywords internal
#' @noRd
setClassUnion("MatrixOrNumeric", c("matrix", "numeric"))

#' An S4 class to represent a Bayes linear object.
#'
#' @name bl
#' @rdname bl
#'
#' @slot name A string for the name of the variable collection.
#' @slot varnames A character vector of variable names.
#' @slot expectation A numeric vector of expectations.
#' @slot covariance A numeric matrix (or a numeric scalar) for covariances.
#'
#' @export
#' @examples
#' bl1 <- bl(
#'   name = "Example 1",
#'   varnames = c("x", "y", "z"),
#'   expectation = c(1, 2, 3),
#'   covariance = matrix(c(
#'     1, 0.5, 0.5,
#'     0.5, 1, 0.5,
#'     0.5, 0.5, 1
#'   ), 3, 3)
#' )
#'
bl <- setClass("bl",
  slots = list(
    name = "character",
    varnames = "character",
    expectation = "numeric",
    covariance = "MatrixOrNumeric"
  ),
  validity = function(object) {
    errors <- character()
    length_vars <- length(object@varnames)

    # 1. Check only one name has been passed
    if (length(object@name) != 1) {
      msg <- paste0("Slot 'name' must have length 1, not ", length(object@name), ".")
      errors <- c(errors, msg)
    }

    # 2. Check for uniqueness in variable names
    if (length_vars != length(unique(object@varnames))) {
      msg <- "All variable names in 'varnames' must be unique."
      errors <- c(errors, msg)
    }

    # 3. Check correct number of expectations
    if (length_vars != length(object@expectation)) {
      msg <- paste0(
        "Slot 'expectation' has length ", length(object@expectation),
        ". It should match the number of variables: ", length_vars, "."
      )
      errors <- c(errors, msg)
    }

    # 4. Check for NAs in expectation
    if (any(is.na(object@expectation))) {
      msg <- "The 'expectation' vector must not contain NA values."
      errors <- c(errors, msg)
    }

    # --- Improved Covariance Validity Checks ---

    # 5. Check for NAs in covariance first
    if (any(is.na(object@covariance))) {
      msg <- "The 'covariance' matrix or value must not contain NA values."
      errors <- c(errors, msg)
    } else {
      # Proceed with checks only if no NAs are present

      if (is.matrix(object@covariance)) {
        # 6. Check dimensions of covariance matrix
        if (!all(dim(object@covariance) == c(length_vars, length_vars))) {
          msg <- paste0(
            "The covariance matrix has dimensions (",
            nrow(object@covariance), ", ", ncol(object@covariance),
            "). It must be a square matrix with dimensions (",
            length_vars, ", ", length_vars, ")."
          )
          errors <- c(errors, msg)
        } else {
          # These checks only make sense for a correctly dimensioned matrix

          # 7. Robust check for symmetry
          if (!isSymmetric(object@covariance, tol = 100 * .Machine$double.eps)) {
            msg <- "The covariance matrix is not symmetric."
            errors <- c(errors, msg)
          }

          # 8. Check for positive semi-definiteness via eigenvalues
          # This is the definitive test for a valid covariance matrix.
          eigenvalues <- eigen(object@covariance, symmetric = TRUE, only.values = TRUE)$values
          if (any(eigenvalues < -1e-8)) {
            msg <- paste0(
              "The covariance matrix is not positive semi-definite. ",
              "It has at least one negative eigenvalue."
            )
            errors <- c(errors, msg)
          }
        }
      } else if (is.numeric(object@covariance)) {
        # This handles the case of a single variable (scalar variance)

        # 9. Check for a single value when there is one variable
        if (length_vars == 1 && length(object@covariance) != 1) {
          msg <- paste0(
            "For a single variable, covariance should be a single numeric value, ",
            "not a vector of length ", length(object@covariance), "."
          )
          errors <- c(errors, msg)
        }

        # 10. Check that scalar variance is non-negative
        if (any(object@covariance < -1e-8)) {
          msg <- "The scalar covariance (variance) must be non-negative."
          errors <- c(errors, msg)
        }
      }
    }

    if (length(errors) == 0) TRUE else errors
  }
)

#' Print method for bl class
#'
#' Displays the contents of a Bayes linear ('bl') object, allowing control
#' over the number of digits shown. Called automatically when an object
#' of class 'bl' is printed to the console.
#'
#' @param x The object of class 'bl' to print.
#' @param digits Minimum number of significant digits to display (default:
#'   `getOption("digits")`). Passed to `round()`.
#' @param ... Further arguments passed to or from other methods (currently unused).
#'
#' @return Invisibly returns the original object `x`.
#'
#' @export
#' @importFrom methods show # Good practice to import generics used
#' @importFrom utils head # If you were using head(), etc.
#' @examples
#' bl1 <- bl(
#'   name = "Example 1",
#'   varnames = c("x", "y", "z"),
#'   expectation = c(1.12345, 2.0, 3.987),
#'   covariance = matrix(c(
#'     1.555, 0.5, 0.25,
#'     0.5, 1.111, 0.5,
#'     0.25, 0.5, 1.0
#'   ), 3, 3)
#' )
#'
#' # Default printing (uses options("digits"))
#' bl1
#' print(bl1)
#'
#' # Print with specific digits
#' print(bl1, digits = 2)
#' print(bl1, digits = 4)
#'
setMethod(
  "print",
  "bl",
  function(x, digits = 2, ...) {
    cat(x@name, "\n") # Use x instead of object, as per signature
    cat("\nExpectation:\n")

    # Use the digits argument for rounding
    prmatrix(
      matrix(
        round(x@expectation, digits = digits),
        length(x@varnames), 1
      ),
      rowlab = x@varnames,
      collab = " ",
      quote = FALSE
    ) # Added quote=FALSE for cleaner output

    cat("\nCovariance:\n\n")
    if (length(x@varnames) > 1 && is.matrix(x@covariance)) {
      # Use the digits argument for rounding
      prmatrix(round(x@covariance, digits = digits),
        rowlab = x@varnames,
        collab = x@varnames,
        quote = FALSE
      ) # Added quote=FALSE
    } else if (length(x@varnames) == 1) {
      # Handle scalar case, using digits
      cat(x@varnames, "  ", round(x@covariance, digits = digits), "\n")
    } else {
      cat("[Covariance matrix not available or dimension mismatch]\n") # Handle unexpected cases
    }
    invisible(x) # Standard practice for print methods
  }
)

#' Basic show method for bl class
#'
#' Displays the contents of a Bayes linear ('bl') object. Called automatically
#' when an object of class 'bl' is shown in the console.
#'
#' @param object The object of class 'bl' to show.
#'
#' @return Invisibly returns the original object `object`.
#'
#' @export
setMethod(
  "show",
  "bl",
  function(object) {
    # Calls the print method with default digits
    print(object)
  }
)

#' Plot method for bl class ('plot')
#'
#' Plots the covariance matrix stored in a 'bl' object. If the covariance
#' matrix is a scalar, a message is printed to the console.
#'
#' @param x The object of class 'bl' to plot.
#'
#' @return NULL
#'
#' @export
setMethod(
  "plot",
  "bl",
  function(x) {
    # Save current parameters
    old_par <- par(no.readonly = TRUE)
    # Ensure parameters are restored when function exits, even if an error occurs
    on.exit(par(old_par), add = TRUE)

    # Set margins to make room for the legend
    par(mar = c(5.1, 4.1, 4.1, 4.1))
    if (is.matrix(x@covariance)) {
      # Set row and column names for the covariance matrix
      colnames(x@covariance) <- x@varnames
      rownames(x@covariance) <- x@varnames
      # Plot covariance matrix forcing 0 to be in the legend and
      # making negative values shades of blue and positive values
      # shades of red.
      # Take the breaks out of the legend and reduce the number of
      # values shown on the legend.
      plot(
        x = x@covariance,
        main = paste0(
          "Covariance stored in ",
          x@name
        ),
        xlab = "",
        ylab = "",
        col = colorRampPalette(c("blue", "white", "red"))(50),
        breaks = seq(-max(abs(x@covariance)),
          max(abs(x@covariance)),
          length.out = 50
        ),
        border = NA,
        key = list(
          side = 4,
          font = 2,
          cex.axis = 0.75
        ),
        fmt.key = "%.2f",
        polygon.key = NULL,
        axis.key = NULL,
        spacing.key = c(1, 0.11, -0.5),
        cex = 0.5
      )
    } else {
      cat("No point in plotting a scalar covariance.\n")
    }
  }
)
