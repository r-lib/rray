# Array titles

Titles are the names on an array's list of names. They appear in array
print output.

- `rray_titles()` returns the titles for every axis.

- `rray_axis_title()` returns the title for one axis.

- `rray_set_titles()` sets the titles for every axis.

- `rray_set_axis_title()` sets the title for one axis.

## Usage

``` r
rray_titles(x)

rray_axis_title(x, axis)

rray_set_titles(x, titles)

rray_set_axis_title(x, axis, title)
```

## Arguments

- x:

  An array.

- axis:

  The axis to get or set the title for.

  - For `rray_set_axis_title()`, given [by position or by
    title](https://rray.r-lib.org/dev/reference/faq-axes.md).

  - For `rray_axis_title()`, given [by
    position](https://rray.r-lib.org/dev/reference/faq-axes.md).

- titles:

  A character vector with one title per axis, or `NULL` to remove all
  titles.

- title:

  A single character string, or `NULL` to remove the title for `axis`.

## Value

- `rray_titles()` returns a character vector, or `NULL` if there are no
  titles.

- `rray_axis_title()` returns one character string, or `NULL` if there
  are no titles. It returns `""` when the titles vector exists but that
  axis has no title.

- The setters return `x` with new titles. Plain vectors become
  one-dimensional arrays.

## Examples

``` r
x <- matrix(1:6, 2, 3)
x <- rray_set_titles(x, c("Row", "Column"))
rray_titles(x)
#> [1] "Row"    "Column"
rray_axis_title(x, 2)
#> [1] "Column"
rray_set_axis_title(x, 1, NULL)
#>       Column
#>        [,1] [,2] [,3]
#>   [1,]    1    3    5
#>   [2,]    2    4    6
rray_set_axis_title(x, "Row", "R")
#>       Column
#> R      [,1] [,2] [,3]
#>   [1,]    1    3    5
#>   [2,]    2    4    6
```
