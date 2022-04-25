obj <- bl(name = 'All',
          varnames = c('A','B','F'),
          expectation = c(0,1,6),
          covariance = matrix(runif(9),3,3))

odata <- bl_data(name = 'Data',
                 varnames = c('A','B','F'),
                 values = c(1,2,10))

test_that("Check object class is S4", {
  expect_equal(isS4(obj), TRUE)
  expect_equal(isS4(odata), TRUE)
})
