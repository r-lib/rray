# Remove array axes

`rray_remove_axes()` removes axes with a dimension of 1.

## Usage

``` r
rray_remove_axes(x, axes)
```

## Arguments

- x:

  An array.

- axes:

  An integer vector of axes to remove. Each selected axis must have a
  dimension of 1. At least one axis must remain.

## Value

An array with the selected `axes` removed.

## Details

Removed axes lose their names. Surviving axes keep their names and carry
them to their new locations.

## Examples

``` r
x <- array(1:10, c(10, 1, 1))

rray_remove_axes(x, 2)
#>       [,1]
#>  [1,]    1
#>  [2,]    2
#>  [3,]    3
#>  [4,]    4
#>  [5,]    5
#>  [6,]    6
#>  [7,]    7
#>  [8,]    8
#>  [9,]    9
#> [10,]   10
rray_remove_axes(x, c(2, 3))
#>  [1]  1  2  3  4  5  6  7  8  9 10
```
