% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bl.R
\name{print,bl-method}
\alias{print,bl-method}
\title{Print method for bl class}
\usage{
\S4method{print}{bl}(x, digits = 2, ...)
}
\arguments{
\item{x}{The object of class 'bl' to print.}

\item{digits}{Minimum number of significant digits to display (default:
\code{getOption("digits")}). Passed to \code{round()}.}

\item{...}{Further arguments passed to or from other methods (currently unused).}
}
\value{
Invisibly returns the original object \code{x}.
}
\description{
Displays the contents of a Bayes linear ('bl') object, allowing control
over the number of digits shown. Called automatically when an object
of class 'bl' is printed to the console.
}
\examples{
bl1 <- bl(
  name = "Example 1",
  varnames = c("x", "y", "z"),
  expectation = c(1.12345, 2.0, 3.987),
  covariance = matrix(c(
    1.555, 0.5, 0.25,
    0.5, 1.111, 0.5,
    0.25, 0.5, 1.0
  ), 3, 3)
)

# Default printing (uses options("digits"))
bl1
print(bl1)

# Print with specific digits
print(bl1, digits = 2)
print(bl1, digits = 4)

}
