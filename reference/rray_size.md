# Find the size of an array

`rray_size()` returns the total number of elements in an array, computed
as the product of its dimensions.

## Usage

``` r
rray_size(x)
```

## Arguments

- x:

  An array.

## Value

A single double representing the total number of elements.

## Examples

``` r
rray_size(1:5)
#> [1] 5
rray_size(array(1, c(2, 3)))
#> [1] 6
rray_size(array(1, c(2, 3, 4)))
#> [1] 24
```
