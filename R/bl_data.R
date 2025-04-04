#' An S4 class to represent data for a Bayes linear adjustment.
#'
#' @name bl_data
#' @rdname bl_data
#'
#' @slot name A string for the name of the variable collection.
#' @slot varnames A character vector of variable names.
#' @slot values A numeric vector of observed values (must be finite and not NA).
#'
#' @export
#' @examples
#' bl_data(name = 'Example 1',
#'        varnames = c('x', 'y', 'z'),
#'        values = c(1, 2, 3))
bl_data <- setClass('bl_data',
                    slots = list(name = 'character',
                                 varnames = 'character',
                                 values = 'numeric'),
                    validity =  function(object) {
                      # Somewhere to store error messages
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

                      # Check for finite values and absence of NA
                      if (any(!is.finite(object@values))) {
                        msg <- "All values must be finite (numeric, not NA, Inf, or -Inf)."
                        errors <- c(errors, msg)
                      }

                      # Check correct number of values
                      length_vars <- length(object@varnames)
                      if (length_vars != length(object@values)) {
                        msg <- paste0("Values is length ",
                                      length(object@values),
                                      ".  Should be ",
                                      length_vars,
                                      ".")
                        errors <- c(errors, msg)
                      }

                      # Check for finite values and absence of NA
                      if (any(!is.finite(object@values))) {
                        msg <- "All values must be finite (numeric, not NA, Inf, or -Inf)."
                        # Find which ones are not finite to potentially make msg more specific
                        # non_finite_idx <- which(!is.finite(object@values))
                        # msg <- paste0("Non-finite values found at indices: ", paste(non_finite_idx, collapse=", "))
                        errors <- c(errors, msg)
                      }

                      if (length(errors) == 0) TRUE else errors
                    }
)

#' Print method for bl_data class
#'
#' Displays the observed values stored in a Bayes linear data ('bl_data') object,
#' allowing control over the number of digits shown. Called automatically when
#' an object of class 'bl_data' is printed to the console.
#'
#' @param x The object of class 'bl_data' to print.
#' @param digits Minimum number of significant digits to display (default:
#'   `getOption("digits")`). Passed to `round()`.
#' @param ... Further arguments passed to or from other methods (currently unused).
#'
#' @return Invisibly returns the original object `x`.
#'
#' @export
#' @importFrom utils prmatrix # Import functions used
#' @examples
#' data1 <- bl_data(name = 'Experiment A',
#'                  varnames = c('Temp', 'Pressure'),
#'                  values = c(25.123, 1013.456))
#' data1
#' print(data1, digits = 4)
#'
setMethod('print',
          'bl_data',
          function(x, digits = getOption("digits"), ...) {
            cat(x@name, '\n')
            cat('Observed values:\n')

            # Use the digits argument for rounding
            prmatrix(matrix(round(x@values, digits = digits),
                            length(x@varnames), 1),
                     rowlab = x@varnames,
                     collab = ' ',
                     quote = FALSE) # Added quote=FALSE

            invisible(x) # Standard practice for print methods
          }
)

#' Basic show method for bl_data class
#'
#' Displays the contents of a Bayes linear data ('bl_data') object.
#'
#' @param object The object of class 'bl_data' to show.
#' @return Invisibly returns the original object `object`.
#' @export
#' @importFrom methods show # Import generic
setMethod('show',
          'bl_data',
          function(object) {
            # Calls the print method with default digits
            print(object)
          }
)

#' Plot method for bl_data class ('plot')
#'
#' @export
setMethod('plot',
          'bl_data',
          function(x) {
            par(mar=c(5.1, 4.1, 4.1, 2.1))
            plot(x = 1:length(x@varnames),
                 y = x@values,
                 main = paste0('Observed values from ',
                               x@name),
                 xlab = 'Variables',
                 ylab = 'Values',
                 pch = 19,
                 xaxt = 'n')
            axis(side = 1,
                 at = 1:length(x@varnames),
                 labels = x@varnames)
          })
