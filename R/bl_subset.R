#' bl object subsetting
#'
#' @param x bl object to be subsetted.
#' @param varnames variable names to be extracted.
#'
#' @return Extracted bl object.
#' @export
bl_subset <- function(x,
                      varnames){
  # Somewhere to store error messages
  errors <- character()
  
  # Check x is a BL or BL data object
  if (!(class(x)[1] %in% c('bl','bl_data'))){
    msg <- paste0("The class of x is ",
                  class(x)[1],
                  ".  It should be bl or bl_data.")
    errors <- c(errors, msg)
  }
  
  # Return any errors
  if (length(errors) > 0) stop(paste(errors,
                                     '\n  '))
  
  # Check the names are in x
  if(!(all(varnames %in% x@varnames))){
    msg <- paste0("The variables to be extracted are not in x.")
    errors <- c(errors, msg)
  }
  
  # Return any errors
  if (length(errors) > 0) stop(paste(errors,
                                     '\n  '))
  
  if (class(x)[1] == 'bl'){
    # Pick out mean and covariance from x that corresponds 
    # with variables in names.
    x_indices <- sapply(varnames,
                        function(x_names) which(x@varnames %in% x_names))
    y_expectation <- x@expectation[x_indices]
    y_variance <- x@covariance[x_indices, x_indices,
                               drop = FALSE]
    
    # Set up new bl object with extracted mean and variance
    y <- bl(name = paste0(x@name,
                          '_extract'),
            varnames = varnames,
            expectation = y_expectation,
            covariance = y_variance)
  } else {
    # Pick out values from x that corresponds 
    # with variables in names.
    x_indices <- sapply(varnames,
                        function(x_names) which(x@varnames %in% x_names))
    y_values <- x@values[x_indices]
    
    # Set up new bl object with extracted mean and variance
    y <- bl_data(name = paste0(x@name,
                               '_extract'),
                 varnames = varnames,
                 values = y_values)
  }
  
  return(y)
}
