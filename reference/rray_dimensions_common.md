# Find common dimensions

`rray_dimensions_common()` finds the common dimensions among multiple
arrays using broadcasting rules. For each axis, dimensions are
compatible if they are equal or if one of them is 1.

## Usage

``` r
rray_dimensions_common(..., .dimensions = NULL)
```

## Arguments

- ...:

  Arrays.

- .dimensions:

  If provided, an integer vector of dimensions to use as an override,
  rather than computing common dimensions from `...`.

## Value

An integer vector of common dimensions.

## Examples

``` r
rray_dimensions_common(array(1, c(2, 3)), array(1, c(1, 3)))
#> [1] 2 3
rray_dimensions_common(1:5, array(1, c(1, 3)))
#> [1] 5 3
rray_dimensions_common(1:5, .dimensions = c(5L, 3L))
#> [1] 5 3
```
