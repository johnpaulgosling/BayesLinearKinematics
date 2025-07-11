library(BayesLinearKinematics)

# Checking 'bl' object initialisation
cov_help <- matrix(runif(9), 3, 3)
diag(cov_help) <- rep(1, 3)
obj <- bl(
  name = "All",
  varnames = c("A", "B", "F"),
  expectation = c(0, 1, 6),
  covariance = cov_help %*% t(cov_help)
)


isS4(obj)

obj
plot(obj)

# Checking 'bl_data' object initialisation
odata <- bl_data(
  name = "Data",
  varnames = c("A", "B", "F"),
  values = c(1, 2, 10)
)

odata
typeof(odata)
class(odata)
plot(odata)

# Checking simple BL adjustment adjustment
x <- bl(
  name = "All",
  varnames = c("A", "B", "C"),
  expectation = c(0, 1, 6),
  covariance = cov_help %*% t(cov_help)
)
x
bl_subset(x, c("C", "A"))

y <- bl_data(
  name = "Data",
  varnames = c("B", "C"),
  values = c(2, 2)
)
y

x_adj_y <- bl_adjust(x, y)
x_adj_y
plot(x_adj_y)
bl_resolution(x, x_adj_y)

y <- bl_data(
  name = "Data",
  varnames = c("A"),
  values = c(-200)
)
y

x_adj_y <- bl_adjust(x, y)
x_adj_y
plot(x_adj_y)
bl_resolution(x, x_adj_y)

# Checking BLK adjustment
x <- bl(
  name = "All",
  varnames = c("A", "B", "C"),
  expectation = c(0, 1, 0),
  covariance = matrix(runif(9), 3, 3)
)
diag(x@covariance) <- rep(1, 3)
x@covariance <- x@covariance %*% t(x@covariance)
x

y <- bl(
  name = "Data",
  varnames = c("B", "C"),
  expectation = c(5, 0),
  covariance = matrix(runif(4) / 4, 2, 2)
)
diag(y@covariance) <- rep(0.5, 2)
y@covariance <- y@covariance %*% t(y@covariance)
y

x_adj_y <- bl_adjust(x, y)
x_adj_y
plot(x_adj_y)
bl_resolution(x, x_adj_y)

# Some error checking
bl_resolution(x, 1)
bl_resolution(x, "a")
bl_resolution(matrix(0, 2, 3), x)
bl_adjust(x, 1)
bl_adjust(x, "a")
bl_adjust(matrix(0, 2, 3), x)
bl_subset(x, 1)
bl_subset(x, "a")
bl_subset(matrix(0, 2, 3), x)

z <- bl(
  name = "Data",
  varnames = c("V", "K"),
  expectation = c(5, 0),
  covariance = matrix(runif(4) / 4, 2, 2)
)
diag(z@covariance) <- rep(0.5, 2)
z@covariance <- z@covariance %*% t(z@covariance)
z
bl_resolution(x, z)
bl_adjust(x, z)

z <- bl(
  name = "Data",
  varnames = c("V", "K"),
  expectation = c(5, 0),
  covariance = diag(-1, 2)
)

z <- bl(
  name = "Data",
  varnames = c("V", "V"),
  expectation = c(5, 0),
  covariance = diag(1, 2)
)
