#' Union class for matrix or numeric
#' @noRd # Usually don't need to export the union itself
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
#' bl1 <- bl(name = 'Example 1',
#'           varnames = c('x', 'y', 'z'),
#'           expectation = c(1, 2, 3),
#'           covariance = matrix(c(1, 0.5, 0.5,
#'                       0.5, 1, 0.5,
#'                       0.5, 0.5, 1), 3, 3))
#'
bl <- setClass('bl',
               slots = list(name = 'character',
                            varnames = 'character',
                            expectation = 'numeric',
                            covariance = 'MatrixOrNumeric'),
               validity = function(object) {
                 errors <- character()

                 # Check only one name has been passed
                 if (length(object@name) != 1){
                   msg <- paste0("Name is length ",
                                 length(object@name),
                                 ". Should be 1.")
                   errors <- c(errors, msg)
                 }

                 # Check for uniqueness in variable names
                 if (length(object@varnames) != length(unique(object@varnames))){
                   msg <- paste0("All variables need to have unique names.")
                   errors <- c(errors, msg)
                 }

                 # Check correct number of expectations
                 length_vars <- length(object@varnames)
                 if (length_vars != length(object@expectation)) {
                   msg <- paste0("Expectation is length ",
                                 length(object@expectation),
                                 ". Should be ",
                                 length_vars,
                                 ".")
                   errors <- c(errors, msg)
                 }

                 if (is.matrix(object@covariance)) {
                   # Check dimensions of covariance
                   row_length <- nrow(object@covariance)
                   col_length <- ncol(object@covariance)
                   if (length_vars != col_length) {
                     msg <- paste0("The number of columns in covariance is ",
                                   col_length,
                                   ". Should be ",
                                   length_vars,
                                   ".")
                     errors <- c(errors, msg)
                   }
                   if (length_vars != row_length) {
                     msg <- paste0("The number of rows in covariance is ",
                                   row_length,
                                   ". Should be ",
                                   length_vars,
                                   ".")
                     errors <- c(errors, msg)
                   }

                   # Check validity of covariance matrix
                   variances <- diag(object@covariance)
                   if (any(variances < -1e-8)){
                     msg <- paste0("The covariance matrix has negative entries on the diagonal.")
                     errors <- c(errors, msg)
                   }

                   # Check symmetry of covariance matrix
                   if (col_length == row_length){
                     if (any(object@covariance != t(object@covariance))){
                       msg <- paste0("The covariance matrix is not symmetric.")
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
#' bl1 <- bl(name = 'Example 1',
#'           varnames = c('x', 'y', 'z'),
#'           expectation = c(1.12345, 2.0, 3.987),
#'           covariance = matrix(c(1.555, 0.5, 0.25,
#'                                 0.5, 1.111, 0.5,
#'                                 0.25, 0.5, 1.0), 3, 3))
#'
#' # Default printing (uses options("digits"))
#' bl1
#' print(bl1)
#'
#' # Print with specific digits
#' print(bl1, digits = 2)
#' print(bl1, digits = 4)
#'
setMethod('print',
          'bl',
          function(x, digits = 2, ...) {
            cat(x@name, '\n') # Use x instead of object, as per signature
            cat('\nExpectation:\n')

            # Use the digits argument for rounding
            prmatrix(matrix(round(x@expectation, digits = digits),
                            length(x@varnames), 1),
                     rowlab = x@varnames,
                     collab = ' ',
                     quote = FALSE) # Added quote=FALSE for cleaner output

            cat('\nCovariance:\n\n')
            if (length(x@varnames) > 1 && is.matrix(x@covariance)) {
              # Use the digits argument for rounding
              prmatrix(round(x@covariance, digits = digits),
                       rowlab = x@varnames,
                       collab = x@varnames,
                       quote = FALSE) # Added quote=FALSE
            } else if (length(x@varnames) == 1) {
              # Handle scalar case, using digits
              cat(x@varnames, '  ', round(x@covariance, digits = digits), '\n')
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
setMethod('show',
          'bl',
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
setMethod('plot',
          'bl',
          function(x) {
            # Save current parameters
            old_par <- par(no.readonly = TRUE)
            # Ensure parameters are restored when function exits, even if an error occurs
            on.exit(par(old_par), add = TRUE)

            # Set margins to make room for the legend
            par(mar=c(5.1, 4.1, 4.1, 4.1))
            if (is.matrix(x@covariance)) {
              # Set row and column names for the covariance matrix
              colnames(x@covariance) <- x@varnames
              rownames(x@covariance) <- x@varnames
              # Plot covariance matrix forcing 0 to be in the legend and
              # making negative values shades of blue and positive values
              # shades of red.
              # Take the breaks out of the legend and reduce the number of
              # values shown on the legend.
              plot(x = x@covariance,
                   main = paste0('Covariance stored in ',
                                 x@name),
                   xlab = '',
                   ylab = '',
                   col = colorRampPalette(c('blue', 'white', 'red'))(50),
                   breaks = seq(-max(abs(x@covariance)),
                                max(abs(x@covariance)),
                                length.out = 50),
                   border = NA,
                   key=list(side=4,
                            font=2,
                            cex.axis=0.75),
                   fmt.key="%.2f",
                   polygon.key=NULL,
                   axis.key=NULL,
                   spacing.key=c(1,0.11,-0.5),
                   cex = 0.5)
            } else {
              cat("No point in plotting a scalar covariance.\n")
            }
          })
