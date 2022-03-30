#' bl object subsetting
#'
#' @param x bl object to be subsetted.
#' @param names variable names to be extracted.
#'
#' @return Extracted bl object.
#' @export
bl_subset <- function(x,
                      names){
  # Check x is a BL object
  if (class(x)[1] != 'bl'){
    msg <- paste0("The class of x is ",
                  class(x)[1],
                  ".  It should be bl.")
    errors <- c(errors, msg)
  }
  
  # Check the names are in x
  if(!(all(names %in% x@varnames))){
    msg <- paste0("The variables to extract are not in x.")
    errors <- c(errors, msg)
  }
  
  # Pick out mean and covariance from x that corresponds 
  # with variables in names.
  x_indices <- sapply(names,
                      function(x_names) which(x@varnames %in% x_names))
  y_expectation <- x@expectation[x_indices]
  y_variance <- x@covariance[x_indices, x_indices]
  
  # Set up new bl object with extracted mean and variance
  y <- bl(name = paste0(x@name,
                        '_extract'),
          varnames = names,
          expectation = y_expectation,
          covariance = y_variance) 
  
  return(y)
}
