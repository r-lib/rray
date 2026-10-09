# Elementwise maximum and minimum

- `rray_pmax()` computes the elementwise maximum of two arrays.

- `rray_pmin()` computes the elementwise minimum of two arrays.

## Usage

``` r
rray_pmax(x, y, ..., na_rm = FALSE)

rray_pmin(x, y, ..., na_rm = FALSE)
```

## Arguments

- x:

  An array.

- y:

  An array.

- ...:

  These dots are for future extensions and must be empty.

- na_rm:

  If `TRUE`, missing values are removed before taking the maximum or
  minimum.

## Value

An array with the common dimensions and common type of `x` and `y`.

## Details

The arrays are broadcast to common dimensions first, so they do not have
to be the same shape.

When `NA` and `NaN` are both present, `NA` wins, like
[`rray_max()`](https://rray.r-lib.org/reference/rray-reduce.md),
[`rray_min()`](https://rray.r-lib.org/reference/rray-reduce.md),
[`max()`](https://rdrr.io/r/base/Extremes.html), and
[`min()`](https://rdrr.io/r/base/Extremes.html). This differs from
[`pmax()`](https://rdrr.io/r/base/Extremes.html) and
[`pmin()`](https://rdrr.io/r/base/Extremes.html), which return whichever
comes last:

    rray_pmax(NA, NaN) # NA
    rray_pmax(NaN, NA) # NA

    pmax(NA, NaN) # NaN

## Examples

``` r
x <- array(1:6, c(3L, 2L))

rray_pmax(x, 4L)
#>      [,1] [,2]
#> [1,]    4    4
#> [2,]    4    5
#> [3,]    4    6
rray_pmin(x, 4L)
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    4
#> [3,]    3    4

# Compare each column with a different value through broadcasting
rray_pmax(x, array(c(2L, 5L), c(1L, 2L)))
#>      [,1] [,2]
#> [1,]    2    5
#> [2,]    2    5
#> [3,]    3    6
```
