library(BayesLinearKinematics)

obj <- BLK(name = 'All',
           varnames = c('A','B','F'),
           expectation = c(0,1,0),
           covariance = matrix(runif(9),3,3))

diag(obj@covariance) <- rep(1,3)

isS4(obj)
obj@covariance <- obj@covariance %*% t(obj@covariance)

obj
plot(obj)
