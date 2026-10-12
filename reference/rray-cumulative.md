# Accumulate along an axis

- `rray_cumulative_all()` checks if all values are `TRUE` so far along
  `axis`.

- `rray_cumulative_any()` checks if any value is `TRUE` so far along
  `axis`.

- `rray_cumulative_max()` computes the maximum so far along `axis`.

- `rray_cumulative_min()` computes the minimum so far along `axis`.

- `rray_cumulative_prod()` computes the product so far along `axis`.

## Usage

``` r
rray_cumulative_all(x, axis)

rray_cumulative_any(x, axis)

rray_cumulative_max(x, axis)

rray_cumulative_min(x, axis)

rray_cumulative_prod(x, axis)
```

## Arguments

- x:

  An array.

- axis:

  A single integer. The axis to accumulate along. `1` accumulates down
  rows, `2` accumulates across columns, and so on.

## Value

An array with the same dimensions and names as `x`.

## Details

Each position along `axis` summarizes every position up to and including
it.

## All / Any

`x` must be logical. Missing values follow
[`all()`](https://rdrr.io/r/base/all.html) and
[`any()`](https://rdrr.io/r/base/any.html), so a later `FALSE` or `TRUE`
can settle an earlier `NA`:

    rray_cumulative_all(c(TRUE, NA, FALSE), 1L) # TRUE, NA, FALSE
    rray_cumulative_any(c(FALSE, NA, TRUE), 1L) # FALSE, NA, TRUE

## Min / Max

`x` must be logical, integer, or double, and the result keeps the type
of `x`. Once a line hits `NA` or `NaN`, the rest of it stays missing. A
later `NA` replaces an earlier `NaN`, like
[`max()`](https://rdrr.io/r/base/Extremes.html) and
[`min()`](https://rdrr.io/r/base/Extremes.html):

    rray_cumulative_max(c(1, NaN, 2, NA, 3), 1L) # 1, NaN, NaN, NA, NA

For logical `x`, a later `TRUE` or `FALSE` does not settle an earlier
`NA`, unlike `rray_cumulative_any()` and `rray_cumulative_all()`:

    rray_cumulative_max(c(NA, TRUE), 1L) # NA, NA
    rray_cumulative_any(c(NA, TRUE), 1L) # NA, TRUE

## Product

`x` must be logical, integer, double, or complex. Logicals and integers
are cast to double.

When `NA` and `NaN` are both present, you get one of them, but which one
depends on the platform and the order of the values. Either way,
[`is.na()`](https://rdrr.io/r/base/NA.html) is `TRUE` from that point
on:

    rray_cumulative_prod(c(2, NA, NaN), 1L) # 2, NA, NA or NaN

## Examples

``` r
x <- array(c(TRUE, TRUE, FALSE, TRUE, NA, TRUE), c(2L, 3L))
x
#>      [,1]  [,2] [,3]
#> [1,] TRUE FALSE   NA
#> [2,] TRUE  TRUE TRUE

# Down rows
rray_cumulative_all(x, 1L)
#>      [,1]  [,2] [,3]
#> [1,] TRUE FALSE   NA
#> [2,] TRUE FALSE   NA
rray_cumulative_any(x, 1L)
#>      [,1]  [,2] [,3]
#> [1,] TRUE FALSE   NA
#> [2,] TRUE  TRUE TRUE

# Across columns
rray_cumulative_all(x, 2L)
#>      [,1]  [,2]  [,3]
#> [1,] TRUE FALSE FALSE
#> [2,] TRUE  TRUE  TRUE
rray_cumulative_any(x, 2L)
#>      [,1] [,2] [,3]
#> [1,] TRUE TRUE TRUE
#> [2,] TRUE TRUE TRUE

x <- array(c(3L, 1L, 2L, 5L, 4L, 0L), c(2L, 3L))
x
#>      [,1] [,2] [,3]
#> [1,]    3    2    4
#> [2,]    1    5    0

# Running maximum and minimum across columns
rray_cumulative_max(x, 2L)
#>      [,1] [,2] [,3]
#> [1,]    3    3    4
#> [2,]    1    5    5
rray_cumulative_min(x, 2L)
#>      [,1] [,2] [,3]
#> [1,]    3    2    2
#> [2,]    1    1    0

# Running product down rows
rray_cumulative_prod(x, 1L)
#>      [,1] [,2] [,3]
#> [1,]    3    2    4
#> [2,]    3   10    0
```
