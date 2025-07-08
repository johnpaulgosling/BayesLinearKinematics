set.seed(233)

cov_help <- matrix(runif(9), 3, 3)
diag(cov_help) <- rep(1, 3)
obj <- bl(
  name = "All",
  varnames = c("A", "B", "F"),
  expectation = c(0, 1, 6),
  covariance = cov_help %*% t(cov_help)
)

sub_obj <- bl_subset(obj, c("F"))

test_that("bl_subset operation extracting one", {
  expect_equal(sub_obj@name, "All_extract")
  expect_equal(sub_obj@expectation, 6)
  expect_equal(round(sub_obj@covariance, 1), matrix(2.1, 1, 1))
})

sub_obj <- bl_subset(obj, c("B", "A"))

test_that("bl_subset operation extracting two", {
  expect_equal(sub_obj@name, "All_extract")
  expect_equal(sub_obj@expectation, c(1, 0))
  expect_equal(round(sub_obj@covariance, 2), matrix(
    c(
      1.69, 1.43,
      1.43, 1.88
    ),
    byrow = TRUE, 2, 2
  ))
})

data_obj <- bl_data(
  name = "My_data",
  varnames = c("A", "B", "F"),
  values = c(1, 2, 6)
)

sub_data_obj <- bl_subset(data_obj, c("B"))

test_that("bl_subset operation extracting two", {
  expect_equal(sub_data_obj@name, "My_data_extract")
  expect_equal(sub_data_obj@values, 2)
})

cov_help <- matrix(runif(9), 3, 3)
diag(cov_help) <- rep(1, 3)
obj_2 <- bl(
  name = "Second",
  varnames = c("F", "A", "B"),
  expectation = c(0, 1, 6),
  covariance = cov_help %*% t(cov_help)
)

sub_obj_2 <- bl_subset(
  obj_2,
  obj@varnames
)

test_that("bl_subset inheriting order from other bl", {
  expect_equal(sub_obj_2@name, "Second_extract")
  expect_equal(sub_obj_2@varnames, obj@varnames)
  expect_equal(sub_obj_2@expectation, c(1, 6, 0))
  expect_equal(round(sub_obj_2@covariance, 1), matrix(
    c(
      2.4, 2.3, 1.4,
      2.3, 2.4, 1.4,
      1.4, 1.4, 1.2
    ),
    byrow = TRUE, 3, 3
  ))
})

# TODO Test needed to check permutation order.

# TODO Additional tests that throw errors.
