#' Bayes linear kinematic adjustment
#'
#' @param x bl object prior to adjustment.
#' @param y bl object after adjustment.
#'
#' @return Vector of resolutions on the [0,1] scale.
#' @export
resolution <- function(x,
                       y){
  # Somewhere to store error messages
  errors <- character()
  
  # Check x and y are BL objects
  if (class(x)[1] != 'bl'){
    msg <- paste0("The class of x is ",
                  class(x)[1],
                  ".  It should be bl.")
    errors <- c(errors, msg)
  }
  if (class(y)[1] != 'bl'){
    msg <- paste0("The class of y is ",
                  class(y)[1],
                  ".  It should be bl.")
    errors <- c(errors, msg)
  }
  
  # Return any errors
  if (length(errors) > 0) return(errors)
  
  # Check x and y are looking at the same variables
  if (any(x@varnames != y@varnames)){
    msg <- paste0("The two bl objects do not contain the same variables.")
    errors <- c(errors, msg)
  }
  
  # Return any errors
  if (length(errors) > 0) return(errors)
  
  # Compute the resolution vector
  resolution <- 1 - diag(y@covariance)/diag(x@covariance)
  
  return(resolution)
}
