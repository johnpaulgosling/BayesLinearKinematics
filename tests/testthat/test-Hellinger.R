x <- bl(name = 'x',
        varnames = c('A','B'),
        expectation = c(0,0),
        covariance = matrix(c(1,0.5,
                              0.5,1),2,2))

y <- bl(name = 'y',
        varnames = c('A','B'),
        expectation = c(1,1),
        covariance = matrix(c(1,0.5,
                              0.5,1),2,2))

h_dist <- hellinger_squared(x, y)
as.numeric(h_dist)

test_that("hellinger_squared operation", {
  expect_equal(round(h_dist,4), 0.1535)
})

h_dist_x <- hellinger_squared(x, x)
h_dist_y <- hellinger_squared(y, y)

test_that("hellinger_squared for equal specifications", {
  expect_equal(h_dist_x, 0)
  expect_equal(h_dist_y, 0)
})

# TODO Additional tests that throw errors.