#' Bayes linear kinematic adjustment
#'
#' @param x bl object prior to adjustment.
#' @param y bl object after adjustment.
#'
#' @return Vector of resolutions on the 0,1 scale.
#' @export
#' @examples
#' bl1 <- bl(name = 'Example 1',
#'          varnames = c('x', 'y', 'z'),
#'          expectation = c(1, 2, 3),
#'          covariance = matrix(c(1, 0.5, 0.5,
#'            0.5, 1, 0.5,
#'            0.5, 0.5, 1), 3, 3))
#'            
#' bl2 <- bl(name = 'Example 2',
#'         varnames = c('x', 'y', 'z'),
#'         expectation = c(1.1, 2.1, 3.1),
#'         covariance = matrix(c(1.1, 0.6, 0.6,
#'         0.6, 1.1, 0.6,
#'         0.6, 0.6, 1.1), 3, 3))
#'         
#' bl_resolution(bl2, bl1)         
bl_resolution <- function(x,
                       y){
  # Somewhere to store error messages
  errors <- character()
  
  # Check x and y are BL objects
  if (class(x)[1] != 'bl'){
    msg <- paste0("The class of x is ",
                  class(x)[1],
                  ". It should be bl.")
    errors <- c(errors, msg)
  }
  if (class(y)[1] != 'bl'){
    msg <- paste0("The class of y is ",
                  class(y)[1],
                  ". It should be bl.")
    errors <- c(errors, msg)
  }
  
  # Return any errors
  if (length(errors) > 0) stop(paste(errors,
                                     '\n  '))
  
  # Check x and y are looking at the same number of variables
  if (length(x@varnames) != length(y@varnames)){
    msg <- paste0("The two bl objects do not contain the same ",
                  "number of variables.")
    errors <- c(errors, msg)
  }
  
  # Return any errors
  if (length(errors) > 0) stop(paste(errors,
                                     '\n  '))
  
  # Check x and y are looking at the same variables
  if (any(x@varnames != y@varnames)){
    msg <- paste0("The two bl objects do not contain the same variables.")
    errors <- c(errors, msg)
  }
  
  # Return any errors
  if (length(errors) > 0) stop(paste(errors,
                                     '\n  '))
  
  # Compute the resolution vector
  return(1 - diag(y@covariance)/diag(x@covariance))
}
