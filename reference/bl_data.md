# An S4 class to represent data for a Bayes linear adjustment.

An S4 class to represent data for a Bayes linear adjustment.

## Slots

- `name`:

  A string for the name of the variable collection.

- `varnames`:

  A character vector of variable names.

- `values`:

  A numeric vector of observed values (must be finite and not NA).

## Examples

``` r
bl_data(
  name = "Example 1",
  varnames = c("x", "y", "z"),
  values = c(1, 2, 3)
)
#> Example 1 
#> Observed values:
#>    
#> x 1
#> y 2
#> z 3
```
