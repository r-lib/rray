# If-else for arrays

`rray_if_else()` selects values from `true` and `false` using a logical
`condition`. When an element of `condition` is `NA`, it selects from
`missing`, or returns a missing value if `missing` is `NULL`.

All supplied inputs are broadcast to common dimensions. The common
output type comes from `true`, `false`, and `missing`.

## Usage

``` r
rray_if_else(condition, true, false, ..., missing = NULL, dimensions = NULL)
```

## Arguments

- condition:

  A logical array.

- true:

  Values to use where `condition` is `TRUE`.

- false:

  Values to use where `condition` is `FALSE`.

- ...:

  These dots are for future extensions and must be empty.

- missing:

  Values to use where `condition` is `NA`. If `NULL`, missing conditions
  produce missing values of the output type.

- dimensions:

  An optional integer vector of output dimensions. When supplied, every
  input must broadcast to these dimensions. Use
  `rray_dimensions(condition)` to keep the shape of `condition`.

## Value

An array with the common dimensions of the inputs, or `dimensions` when
supplied. Its type comes from `true`, `false`, and `missing`. Names are
dropped.

## Examples

``` r
x <- array(1:12, c(3L, 4L))
x[1, 1] <- NA_integer_
y <- array(101:104, c(1L, 4L))
rray_if_else(x > 5L, x, y, missing = 0L)
#>      [,1] [,2] [,3] [,4]
#> [1,]    0  102    7   10
#> [2,]  101  102    8   11
#> [3,]  101    6    9   12

condition <- array(c(TRUE, FALSE), c(2L, 1L))
true <- array(1:6, c(2L, 3L))
false <- array(11:16, c(2L, 3L))
rray_if_else(condition, true, false)
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]   12   14   16
```
