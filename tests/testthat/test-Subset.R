obj <- bl(name = 'All',
          varnames = c('A','B','F'),
          expectation = c(0,1,6),
          covariance = matrix(1:9,3,3))

sub_obj <- bl_subset(obj, c('F'))

test_that("bl_subset operation extracting one", {
  expect_equal(sub_obj@name, "All_extract")
  expect_equal(sub_obj@expectation, 6)
  expect_equal(sub_obj@covariance, matrix(9,1,1))
})

sub_obj <- bl_subset(obj, c('B','A'))

test_that("bl_subset operation extracting two", {
  expect_equal(sub_obj@name, "All_extract")
  expect_equal(sub_obj@expectation, c(1,0))
  expect_equal(sub_obj@covariance, matrix(c(5,2,
                                            4,1),
                                          byrow=TRUE,2,2))
})
