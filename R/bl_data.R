#' An S4 class to represent data for a Bayes linear adjustment.
#'
#' @slot name A string for the name of the variable collection.
#' @slot varnames A character vector of variable names.
#' @slot values A numeric vector of observed values.
# Validity check for class 'bl_data'
check_bl_data <- function(object) {
  # Somewhere to store error messages
  errors <- character()
  
  # Check only one name has been passed
  if (length(object@name) != 1){
    msg <- paste0("Name is length ",
                  length(object@varnames),
                  ".  Should be 1.")
    errors <- c(errors, msg)
  }
  
  # Check correct number of expectations
  length_vars <- length(object@varnames)
  if (length_vars != length(object@values)) {
    msg <- paste0("Values is length ",
                  length(object@values),
                  ".  Should be ",
                  length_vars,
                  ".")
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) TRUE else errors
}

bl_data <- setClass('bl_data',
                    slots = list(name = 'character',
                                 varnames = 'character',
                                 values = 'numeric'),
                    validity = check_bl_data)

# Print method for class ('show')
setMethod('show',
          'bl_data',
          function(object) {
            cat(object@name, '\n')
            cat('Variable names: ', object@varnames, '\n')
            cat('Observed values:', object@values)
          })

# Plot method for class ('plot')
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
