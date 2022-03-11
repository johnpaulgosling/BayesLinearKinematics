#' An S4 class to represent a Bayes linear object.
#'
#' @slot name A string for the name of the variable collection.
#' @slot varnames A character vector of variable names.
#' @slot expectation A numeric vector of expectations.
#' @slot covariance A numeric matrix of covariances.
BLK <- setClass('BLK',
                slots = list(name = 'character',
                             varnames = 'character',
                             expectation = 'numeric',
                             covariance = 'matrix'))

# Print method for class ('show')
setMethod('show',
          'BLK',
          function(object) {
            cat(object@name, '\n')
            cat('Variable names:', object@varnames, '\n')
            cat('Expectation:   ', object@expectation, '\n')
            cat('Covariance:\n')
            prmatrix(object@covariance,
                     rowlab = object@varnames,
                     collab = object@varnames)
          })

# Plot method for class ('plot')
setMethod('plot',
          'BLK',
          function(x) {
            par(mar=c(5.1, 4.1, 4.1, 4.1))
            colnames(x@covariance) <- x@varnames
            rownames(x@covariance) <- x@varnames
            plot(x = x@covariance[length(x@varnames):1,
                                  1:length(x@varnames)],
                 main = paste0('Covariance stored in ',
                               x@name),
                 )
          })
