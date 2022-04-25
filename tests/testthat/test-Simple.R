x <- bl(name = 'x',
        varnames = c('A','B'),
        expectation = c(0,0),
        covariance = matrix(c(1,0.5,
                              0.5,1),2,2))

y <- bl_data(name = 'y',
             varnames = c('B'),
             values = c(1))

xy <- bl_adjust(x, y)

test_that("bl_adjust operation", {
  expect_equal(xy@name, "x_adj_y")
  expect_equal(xy@expectation, c(0.5, 1))
  expect_equal(xy@covariance, matrix(c(0.75,0,
                                       0,0),2,2))
})

z <- bl(name = 'small',
        varnames = 'A',
        expectation = 0,
        covariance = matrix(1,1,1))
