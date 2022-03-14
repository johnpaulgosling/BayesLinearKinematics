#' Bayes linear adjustment
#'
#' @param x bl object to by adjusted.
#' @param y bl or bl_data object to adjust by.
#'
#' @return Adjusted bl object.
#' @export
bl_adjust <- function(x,
                      y){
  # Simple Bayes linear update if y is of class bl_data
  if (class(y)[1] == 'bl_data'){
    # Pick out mean and covariance from x that corresponds with y
    x_indices <- which(x@varnames %in% y@varnames)
    y_expectation <- x@expectation[x_indices]
    y_variance <- x@covariance[x_indices, x_indices]
    xy_covariance <- x@covariance[, x_indices]
    
    # Invert prior variance for y (using generalised inverse from MASS)
    inv_y_variance <- ginv(y_variance)
    
    # Adjusted expectation
    adj_expectation <- x@expectation + 
      xy_covariance %*% inv_y_variance %*% (y@values - y_expectation)
    
    # Adjusted variance
    adj_variance <- x@covariance - 
      xy_covariance %*% inv_y_variance %*% t(xy_covariance)
    
    # Set up new bl object with adjusted mean and variance
    x_adj_y <- bl(name = paste0(x@name,
                                '_adj_',
                                y@name),
                  varnames = x@varnames,
                  expectation = as.numeric(adj_expectation),
                  covariance = adj_variance) 
  }
  
  return(x_adj_y)
}
