# Bayes linear kinematic adjustment

Bayes linear kinematic adjustment

## Usage

``` r
bl_adjust(x, y)
```

## Arguments

- x:

  bl object to be adjusted.

- y:

  bl or bl_data object to adjust by.

## Value

Adjusted bl object.

## Examples

``` r
bl1 <- bl(
  name = "Variables",
  varnames = c("x", "y", "z"),
  expectation = c(1, 2, 3),
  covariance = matrix(c(
    1, 0.5, 0.5,
    0.5, 1, 0.5,
    0.5, 0.5, 1
  ), 3, 3)
)

bl2 <- bl_data(
  name = "Data",
  varnames = c("x", "y"),
  values = c(1.1, 2.1)
)

bl_adjust(bl1, bl2)
#> Variables_adj_Data 
#> 
#> Expectation:
#>       
#> x 1.10
#> y 2.10
#> z 3.07
#> 
#> Covariance:
#> 
#>   x y    z
#> x 0 0 0.00
#> y 0 0 0.00
#> z 0 0 0.67
```
