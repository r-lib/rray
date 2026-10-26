# Permute array axes

`rray_permute_axes()` reorders the axes of an array.

## Usage

``` r
rray_permute_axes(x, axes)
```

## Arguments

- x:

  An array.

- axes:

  The axes of `x` in their new order, given [by position or by
  title](https://r-lib.github.io/rray/dev/reference/faq-axes.md). It
  must use each axis of `x` exactly once.

## Value

An array with the axes of `x` reordered by `axes`.

## Details

Names and titles travel with their axis to its new position.

`rray_permute_axes(x, c(2, 1))` transposes a matrix, like
[`t()`](https://rdrr.io/r/base/t.html).

## See also

[`rray_move_axes()`](https://r-lib.github.io/rray/dev/reference/rray_move_axes.md)

## Examples

``` r
x <- array(1:6, c(2, 3))

# Transpose a matrix
rray_permute_axes(x, c(2, 1))
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    3    4
#> [3,]    5    6

# Transpose a matrix, by title
z <- rray_set_titles(x, c("row", "col"))
rray_permute_axes(z, c("col", "row"))
#>       row
#> col    [,1] [,2]
#>   [1,]    1    2
#>   [2,]    3    4
#>   [3,]    5    6

y <- array(1:24, c(4, 3, 2))

# Reverse every axis
# (4, 3, 2) -> (2, 3, 4)
rray_dimensions(rray_permute_axes(y, c(3, 2, 1)))
#> [1] 2 3 4

# Swap the first two axes, leaving the third alone
# (4, 3, 2) -> (3, 4, 2)
rray_dimensions(rray_permute_axes(y, c(2, 1, 3)))
#> [1] 3 4 2
```
