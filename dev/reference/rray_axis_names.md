# Get names for a single axis of an array

- `rray_axis_names()` returns the names for a single `axis` of an array,
  or `NULL` if that axis has no names.

- `rray_row_names()` and `rray_column_names()` are shortcuts for
  `rray_axis_names(x, 1)` and `rray_axis_names(x, 2)`.

## Usage

``` r
rray_axis_names(x, axis)

rray_row_names(x)

rray_column_names(x)
```

## Arguments

- x:

  An array.

- axis:

  The axis to get names for, given [by position or by
  title](https://rray.r-lib.org/dev/reference/faq-axes.md).

## Value

A character vector of names, or `NULL` if that axis has no names.

## Examples

``` r
x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))

rray_axis_names(x, 1)
#> [1] "r1" "r2"
rray_axis_names(x, 2)
#> [1] "c1" "c2" "c3"

# By title
y <- rray_set_titles(x, c("row", "col"))
rray_axis_names(y, "col")
#> [1] "c1" "c2" "c3"

rray_row_names(x)
#> [1] "r1" "r2"
rray_column_names(x)
#> [1] "c1" "c2" "c3"
```
