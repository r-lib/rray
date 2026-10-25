# Get the dimensions of an array

`rray_dimensions()` returns the dimension of each axis of an array.

## Usage

``` r
rray_dimensions(x)
```

## Arguments

- x:

  An array.

## Value

An integer vector of dimensions.

## Details

For a plain vector without a `dim` attribute, this returns its length as
a single integer (i.e., a 1-dimensional result).

## Examples

``` r
rray_dimensions(1:5)
#> [1] 5
rray_dimensions(array(1, c(2, 3)))
#> [1] 2 3
rray_dimensions(array(1, c(2, 3, 4)))
#> [1] 2 3 4
```
