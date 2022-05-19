#' Squared Hellinger distance between two bl objects 
#'
#' @param x bl object.
#' @param y bl object.
#'
#' @return Hellinger distance on the [0,1] scale.
#' @export
hellinger_squared <- function(x,
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
  if (length(errors) > 0) stop(paste(errors,
                                     '\n  '))
  
  # Check that all the y variables are in x
  if(!(all(y@varnames %in% x@varnames))){
    msg <- paste0("Some of the variables in y are not in x.")
    errors <- c(errors, msg)
  }
  
  # Check that all the y variables are in x
  if(!(all(x@varnames %in% y@varnames))){
    msg <- paste0("Some of the variables in x are not in y.")
    errors <- c(errors, msg)
  }
  
  # Return any errors
  if (length(errors) > 0) stop(paste(errors,
                                     '\n  '))
  
  # Make sure that the bl objects are in the same order.
  # We will inherit the order from x.
  ordered_y <- bl_subset(y,x@varnames)
  
  # Calculate the squared Hellinger distance
  det_part <- (det(x@covariance)^0.25)*(det(y@covariance)^0.25)/
    (det((x@covariance+y@covariance)/2)^0.5)
  exp_part <- -0.125 * t(x@expectation-y@expectation) %*% 
    ginv((x@covariance+y@covariance)/2) %*% 
    (x@expectation-y@expectation)
  squared_H <- as.numeric(1 - det_part * exp(exp_part))
  
  return(squared_H)
}
