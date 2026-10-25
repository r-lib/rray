# Find the dimensionality of an array

`rray_dimensionality()` returns the number of dimensions of an array as
a single integer.

## Usage

``` r
rray_dimensionality(x)
```

## Arguments

- x:

  An array.

## Value

A single integer representing the number of dimensions.

## Examples

``` r
rray_dimensionality(1)
#> [1] 1
rray_dimensionality(array(1, c(2, 3)))
#> [1] 2
rray_dimensionality(array(1, c(2, 3, 4)))
#> [1] 3
```
