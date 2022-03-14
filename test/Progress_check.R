library(BayesLinearKinematics)

# Checking 'bl' object initialisation
obj <- bl(name = 'All',
          varnames = c('A','B','F'),
          expectation = c(0,1,6),
          covariance = matrix(runif(9),3,3))

diag(obj@covariance) <- rep(1,3)

isS4(obj)
obj@covariance <- obj@covariance %*% t(obj@covariance)

obj
plot(obj)

# Checking 'bl_data' object initialisation
odata <- bl_data(name = 'Data',
                 varnames = c('A','B','F'),
                 values = c(1,2,10))

odata
plot(odata)
