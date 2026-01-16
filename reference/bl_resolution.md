# Calculate Bayes linear variance resolution

Computes the resolution for each variable, defined as
`1 - Var_adjusted / Var_prior`, based on prior and adjusted beliefs.

## Usage

``` r
bl_resolution(x, y)
```

## Arguments

- x:

  A 'bl' object representing the **prior** beliefs (before adjustment).

- y:

  A 'bl' object representing the **adjusted** beliefs (after
  adjustment).

## Value

A named numeric vector of resolutions, ideally on the 0-1 scale. Names
correspond to the variable names in the input objects. Issues a warning
if any prior variances are non-positive or if calculated resolutions
fall outside 0-1.

## Details

The resolution measures the proportion of prior variance removed by the
adjustment process for each variable. It should ideally range from 0 (no
variance reduction) to 1 (variance reduced to zero).

## Examples

``` r
# Prior Beliefs
bl_prior <- bl(
  name = "Prior State",
  varnames = c("P1", "P2", "P3"),
  expectation = c(10, 20, 30),
  covariance = matrix(c(
    4, 1, 0.5,
    1, 5, 1,
    0.5, 1, 6
  ), 3, 3)
)

# Adjusted Beliefs (after some adjustment process)
bl_adjusted <- bl(
  name = "Adjusted State",
  varnames = c("P1", "P2", "P3"),
  expectation = c(11, 20.5, 29),
  covariance = matrix(c(
    2, 0.5, 0.2,
    0.5, 2.5, 0.5,
    0.2, 0.5, 3
  ), 3, 3)
)

# Calculate resolution
resolutions <- bl_resolution(bl_prior, bl_adjusted)
print(resolutions)
#>  P1  P2  P3 
#> 0.5 0.5 0.5 
# Should output something like:
# P1  P2  P3
# 0.5 0.5 0.5

# Example with potential issues (e.g., zero prior variance - may warn/error)
bl_prior_zero_var <- bl(
  name = "Prior with Zero Var",
  varnames = "Z", expectation = 0, covariance = 0
)
bl_adjusted_zero_var <- bl(
  name = "Adjusted with Zero Var",
  varnames = "Z", expectation = 1, covariance = 0.1
)
# try(bl_resolution(bl_prior_zero_var, bl_adjusted_zero_var))
```
