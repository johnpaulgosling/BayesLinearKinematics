cov_help <-  matrix(runif(9),3,3)
diag(cov_help) <- rep(1,3)
obj <- bl(name = 'All',
          varnames = c('A','B','F'),
          expectation = c(0,1,6),
          covariance = cov_help %*% t(cov_help))

odata <- bl_data(name = 'Data',
                 varnames = c('A','B','F'),
                 values = c(1,2,10))

test_that("Check object class is S4", {
  expect_equal(isS4(obj), TRUE)
  expect_equal(isS4(odata), TRUE)
})

test_that("Validity checks for bl class", {
  # Test for name length
  expect_error(bl(name = c('All', 'Another'),
                  varnames = c('A','B','F'),
                  expectation = c(0,1,6),
                  covariance = matrix(runif(9),3,3)),
               "Name is length 2. Should be 1.")
  
  # Test for unique variable names
  expect_error(bl(name = 'All',
                  varnames = c('A','B','A'),
                  expectation = c(0,1,6),
                  covariance = matrix(runif(9),3,3)),
               "All variables need to have unique names.")
  
  # Test for correct number of expectations
  expect_error(bl(name = 'All',
                  varnames = c('A','B','F'),
                  expectation = c(0,1),
                  covariance = matrix(runif(9),3,3)),
               "Expectations is length 2.  Should be 3.")
  
  # Test for covariance matrix validity
  expect_error(bl(name = 'All',
                  varnames = c('A','B','F'),
                  expectation = c(0,1,6),
                  covariance = matrix(c(1,2,3,4,5,6), 2, 3)),
               "The covariance matrix is 3 by 2.  It should be square.")
  
  # Test for covariance matrix symmetry
  # expect_error(bl(name = 'All',
  #                 varnames = c('A','B','F'),
  #                 expectation = c(0,1,6),
  #                 covariance = matrix(c(1,2,3,4,5,6,7,8,9), 3, 3)),
  #              "The covariance matrix is not symmetric.")
  
  # Test for variance validity
  expect_error(bl(name = 'All',
                  varnames = c('A','B','F'),
                  expectation = c(0,1,6),
                  covariance = matrix(c(-1,-2,-3,-4,5,6,-7,8,9), 3, 3)),
               "The covariance matrix has negative entries on the diagonal.")
  
})

test_that("Validity checks for bl_data class", {
  # Test for name length
  expect_error(bl_data(name = c('Data', 'Another'),
                       varnames = c('A','B','F'),
                       values = c(1,2,10)),
               "Name is length 2. Should be 1.")
  
  # Test for unique variable names
  expect_error(bl_data(name = 'Data',
                       varnames = c('A','B','A'),
                       values = c(1,2,10)),
               "All variables need to have unique names.")
  
  # Test for correct number of values
  expect_error(bl_data(name = 'Data',
                       varnames = c('A','B','F'),
                       values = c(1,2)),
               "Values is length")
  
})