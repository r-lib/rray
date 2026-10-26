# Set names for a single axis of an array

- `rray_set_axis_names()` sets the names for a single `axis` of an
  array, leaving every other axis untouched. The titles of `x` are kept.

- `rray_set_row_names()` and `rray_set_column_names()` are shortcuts for
  `rray_set_axis_names(x, 1, names)` and
  `rray_set_axis_names(x, 2, names)`.

## Usage

``` r
rray_set_axis_names(x, axis, names)

rray_set_row_names(x, names)

rray_set_column_names(x, names)
```

## Arguments

- x:

  An array.

- axis:

  The axis to set names for, given [by position or by
  title](https://r-lib.github.io/rray/dev/reference/faq-axes.md).

- names:

  A character vector of names for `axis`, the same length as the
  dimension of `axis`. Can also be `NULL` to remove names from `axis`.

## Value

`x` with new names for `axis`.

## Examples

``` r
x <- array(1:6, c(2, 3))

rray_set_axis_names(x, 1, c("r1", "r2"))
#>    [,1] [,2] [,3]
#> r1    1    3    5
#> r2    2    4    6
rray_set_axis_names(x, 2, c("c1", "c2", "c3"))
#>      c1 c2 c3
#> [1,]  1  3  5
#> [2,]  2  4  6

# By title
y <- rray_set_titles(x, c("row", "col"))
rray_set_axis_names(y, "col", c("c1", "c2", "c3"))
#>       col
#> row    c1 c2 c3
#>   [1,]  1  3  5
#>   [2,]  2  4  6

rray_set_row_names(x, c("r1", "r2"))
#>    [,1] [,2] [,3]
#> r1    1    3    5
#> r2    2    4    6
rray_set_column_names(x, c("c1", "c2", "c3"))
#>      c1 c2 c3
#> [1,]  1  3  5
#> [2,]  2  4  6
```
