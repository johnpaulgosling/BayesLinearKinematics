# Print method for bl_data class

Displays the observed values stored in a Bayes linear data ('bl_data')
object allowing control over the number of digits shown. Called
automatically when an object of class 'bl_data' is printed to the
console.

## Usage

``` r
# S4 method for class 'bl_data'
print(x, digits = getOption("digits"), ...)
```

## Arguments

- x:

  The object of class 'bl_data' to print.

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
data1 <- bl_data(
  name = "Experiment A",
  varnames = c("Temp", "Pressure"),
  values = c(25.123, 1013.456)
)
data1
#> Experiment A 
#> Observed values:
#>                  
#> Temp       25.123
#> Pressure 1013.456
print(data1, digits = 4)
#> Experiment A 
#> Observed values:
#>                  
#> Temp       25.123
#> Pressure 1013.456
```
