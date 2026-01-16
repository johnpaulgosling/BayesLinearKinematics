# Print method for bl class

Displays the contents of a Bayes linear ('bl') object, allowing control
over the number of digits shown. Called automatically when an object of
class 'bl' is printed to the console.

## Usage

``` r
# S4 method for class 'bl'
print(x, digits = 2, ...)
```

## Arguments

- x:

  The object of class 'bl' to print.

- digits:

  Minimum number of significant digits to display (default:
  `getOption("digits")`). Passed to
  [`round()`](https://rdrr.io/r/base/Round.html).

- ...:

  Further arguments passed to or from other methods (currently unused).

## Value

Invisibly returns the original object `x`.

## Examples

``` r
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
#> Example 1 
#> 
#> Expectation:
#>       
#> x 1.12
#> y 2.00
#> z 3.99
#> 
#> Covariance:
#> 
#>      x    y    z
#> x 1.55 0.50 0.25
#> y 0.50 1.11 0.50
#> z 0.25 0.50 1.00
print(bl1)
#> Example 1 
#> 
#> Expectation:
#>       
#> x 1.12
#> y 2.00
#> z 3.99
#> 
#> Covariance:
#> 
#>      x    y    z
#> x 1.55 0.50 0.25
#> y 0.50 1.11 0.50
#> z 0.25 0.50 1.00

# Print with specific digits
print(bl1, digits = 2)
#> Example 1 
#> 
#> Expectation:
#>       
#> x 1.12
#> y 2.00
#> z 3.99
#> 
#> Covariance:
#> 
#>      x    y    z
#> x 1.55 0.50 0.25
#> y 0.50 1.11 0.50
#> z 0.25 0.50 1.00
print(bl1, digits = 4)
#> Example 1 
#> 
#> Expectation:
#>         
#> x 1.1235
#> y 2.0000
#> z 3.9870
#> 
#> Covariance:
#> 
#>       x     y    z
#> x 1.555 0.500 0.25
#> y 0.500 1.111 0.50
#> z 0.250 0.500 1.00
```
