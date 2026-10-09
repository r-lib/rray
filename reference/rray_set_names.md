# Set names for every axis of an array

`rray_set_names()` sets the names for every axis of an array at once.

## Usage

``` r
rray_set_names(x, names)
```

## Arguments

- x:

  An array.

- names:

  A list with length equal to the dimensionality of `x`, where each
  element is either a character vector of names for that axis or `NULL`.
  Can also be `NULL` to remove all names from `x`.

## Value

`x` with new names.

## Examples

``` r
x <- array(1:6, c(2, 3))

rray_set_names(x, list(c("r1", "r2"), c("c1", "c2", "c3")))
#>    c1 c2 c3
#> r1  1  3  5
#> r2  2  4  6

# `NULL` clears all names
y <- rray_set_names(x, list(c("r1", "r2"), NULL))
rray_set_names(y, NULL)
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
```
