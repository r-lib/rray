# Get names for each axis of an array

`rray_names()` returns the names for each axis of an array, or `NULL` if
`x` has no names at all. Unlike
[`dimnames()`](https://rdrr.io/r/base/dimnames.html), it returns a
one-element list for named vectors.

## Usage

``` r
rray_names(x)
```

## Arguments

- x:

  An array.

## Value

Either:

- A list with length equal to the dimensionality of `x`, where each
  element is either a character vector of names or `NULL`.

- `NULL` if `x` has no names.

## Examples

``` r
# No names at all
rray_names(1:3)
#> NULL
# Named vectors return a one-element list
rray_names(c(a = 1, b = 2))
#> [[1]]
#> [1] "a" "b"
#> 

# All dimension names
x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
rray_names(x)
#> [[1]]
#> [1] "r1" "r2"
#> 
#> [[2]]
#> [1] "c1" "c2" "c3"
#> 
```
