# An S4 class to represent a Bayes linear object.

An S4 class to represent a Bayes linear object.

## Slots

- `name`:

  A string for the name of the variable collection.

- `varnames`:

  A character vector of variable names.

- `expectation`:

  A numeric vector of expectations.

- `covariance`:

  A numeric matrix (or a numeric scalar) for covariances.

## Examples

``` r
bl1 <- bl(
  name = "Example 1",
  varnames = c("x", "y", "z"),
  expectation = c(1, 2, 3),
  covariance = matrix(c(
    1, 0.5, 0.5,
    0.5, 1, 0.5,
    0.5, 0.5, 1
  ), 3, 3)
)
```
