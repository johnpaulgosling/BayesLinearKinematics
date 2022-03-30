#' An S4 class to represent a Bayes linear object.
#'
#' @slot name A string for the name of the variable collection.
#' @slot varnames A character vector of variable names.
#' @slot expectation A numeric vector of expectations.
#' @slot covariance A numeric matrix of covariances.
# Validity check for class 'bl'
check_bl <- function(object) {
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
  if (length_vars != length(object@expectation)) {
    msg <- paste0("Expectations is length ",
                  length(object@expectation),
                  ".  Should be ",
                  length_vars,
                  ".")
    errors <- c(errors, msg)
  }
  
  # Check dimensions of covariance
  row_length <- length(object@covariance[1,])
  col_length <- length(object@covariance[,1])
  if (length_vars != row_length) {
    msg <- paste0("The number of columns in covariance is ",
                  row_length,
                  ".  Should be ",
                  length_vars,
                  ".")
    errors <- c(errors, msg)
  }
  if (length_vars != col_length) {
    msg <- paste0("The number of rows in covariance is ",
                  col_length,
                  ".  Should be ",
                  length_vars,
                  ".")
    errors <- c(errors, msg)
  }
  if (col_length != row_length) {
    msg <- paste0("The covariance matrix is ",
                  row_length,
                  " by ",
                  col_length,
                  ".  It should be square.")
    errors <- c(errors, msg)
  }
  
  if (length(errors) == 0) TRUE else errors
}

bl <- setClass('bl',
               slots = list(name = 'character',
                            varnames = 'character',
                            expectation = 'numeric',
                            covariance = 'matrix'),
               validity = check_bl)

# Print method for class ('show')
setMethod('show',
          'bl',
          function(object) {
            cat(object@name, '\n')
            cat('\nExpectation:\n')
            prmatrix(matrix(round(object@expectation, 2),
                            length(object@varnames), 1),
                     rowlab = object@varnames,
                     collab = ' ')
            cat('\nCovariance:\n\n')
            prmatrix(round(object@covariance, 2),
                     rowlab = object@varnames,
                     collab = object@varnames)
          })

# Plot method for class ('plot')
setMethod('plot',
          'bl',
          function(x) {
            par(mar=c(5.1, 4.1, 4.1, 4.1))
            colnames(x@covariance) <- x@varnames
            rownames(x@covariance) <- x@varnames
            plot(x = x@covariance,
                 main = paste0('Covariance stored in ',
                               x@name),
                 xlab = '',
                 ylab = '')
          })
