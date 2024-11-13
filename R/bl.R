#' An S4 class to represent a Bayes linear object.
#'
#' @name bl
#' @rdname bl
#'
#' @slot name A string for the name of the variable collection.
#' @slot varnames A character vector of variable names.
#' @slot expectation A numeric vector of expectations.
#' @slot covariance A numeric matrix or a numeric scalar for covariances.
#'
#' @export
bl <- setClass('bl',
               slots = list(name = 'character',
                            varnames = 'character',
                            expectation = 'numeric',
                            covariance = 'ANY'),
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
                   msg <- paste0("Expectations is length ",
                                 length(object@expectation),
                                 ".  Should be ",
                                 length_vars,
                                 ".")
                   errors <- c(errors, msg)
                 }

                 if (is.matrix(object@covariance)) {
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

#' Print method for bl class ('show')
#'
#' @export
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
            if (length(object@varnames) > 1){
              prmatrix(round(object@covariance, 2),
                       rowlab = object@varnames,
                       collab = object@varnames)} else {
                         cat(object@varnames, '  ',
                             round(object@covariance, 2))
                       }
          }
)

#' Plot method for bl class ('plot')
#'
#' @export
setMethod('plot',
          'bl',
          function(x) {
            par(mar=c(5.1, 4.1, 4.1, 4.1))
            if (is.matrix(x@covariance)) {
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
