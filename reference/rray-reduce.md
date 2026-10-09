# Reduce an array along axes

- `rray_sum()` computes the sum along the specified `axes`.

- `rray_prod()` computes the product along the specified `axes`.

- `rray_mean()` computes the mean along the specified `axes`.

- `rray_all()` checks if all values are `TRUE` along the specified
  `axes`.

- `rray_any()` checks if any value is `TRUE` along the specified `axes`.

- `rray_max()` computes the maximum along the specified `axes`.

- `rray_min()` computes the minimum along the specified `axes`.

## Usage

``` r
rray_sum(x, axes, ..., na_rm = FALSE)

rray_prod(x, axes, ..., na_rm = FALSE)

rray_mean(x, axes, ..., na_rm = FALSE)

rray_all(x, axes, ..., na_rm = FALSE)

rray_any(x, axes, ..., na_rm = FALSE)

rray_max(x, axes, ..., na_rm = FALSE)

rray_min(x, axes, ..., na_rm = FALSE)
```

## Arguments

- x:

  An array.

- axes:

  An integer vector of axes to reduce over. `1` reduces rows, `2`
  reduces columns, and so on.

- ...:

  These dots are for future extensions and must be empty.

- na_rm:

  If `TRUE`, missing values are removed before reducing.

## Value

An array with the same dimensionality as `x`, but with the dimensions
along `axes` reduced to 1.

## Details

The dimensionality of `x` is retained in the result, with the reduced
axes collapsed to size 1.

## Sum

Logicals are summed as integers. If an integer sum doesn't fit in an
integer, an error is thrown. Only the final sum is checked, and `NA`
wins over an overflow:

    x <- c(.Machine$integer.max, 1L, -1L)
    rray_sum(x, 1L) # .Machine$integer.max

    x <- c(.Machine$integer.max, 1L, NA)
    rray_sum(x, 1L) # NA

## Product

Logicals and integers are cast to double.

## Mean

Logicals and integers are cast to double.

When `NA` and `NaN` are both present, you get one of them, but which one
depends on the platform and the order of the values, like
[`mean()`](https://rdrr.io/r/base/mean.html). Either way,
[`is.na()`](https://rdrr.io/r/base/NA.html) is `TRUE`:

    rray_mean(c(NA, NaN), 1L) # NA or NaN
    rray_mean(c(NaN, NA), 1L) # NA or NaN

## Min / Max

`rray_max()` and `rray_min()` keep the type of `x`. With nothing to
reduce, such as an axis of dimension 0, `rray_max()` returns the
smallest value of that type and `rray_min()` returns the largest:

|         |                         |                        |
|---------|-------------------------|------------------------|
| type    | `rray_max()`            | `rray_min()`           |
| logical | `FALSE`                 | `TRUE`                 |
| integer | `-.Machine$integer.max` | `.Machine$integer.max` |
| double  | `-Inf`                  | `Inf`                  |

When `NA` and `NaN` are both present, `NA` wins, like
[`max()`](https://rdrr.io/r/base/Extremes.html) and
[`min()`](https://rdrr.io/r/base/Extremes.html):

    rray_max(c(NA, NaN), 1L) # NA
    rray_max(c(NaN, NA), 1L) # NA

## Examples

``` r
x <- array(1:10, c(5L, 2L))

# Sum along rows
rray_sum(x, 1L)
#>      [,1] [,2]
#> [1,]   15   40

# Sum along columns
rray_sum(x, 2L)
#>      [,1]
#> [1,]    7
#> [2,]    9
#> [3,]   11
#> [4,]   13
#> [5,]   15

# Sum along both axes
rray_sum(x, c(1L, 2L))
#>      [,1]
#> [1,]   55

# Product along rows
rray_prod(x, 1L)
#>      [,1]  [,2]
#> [1,]  120 30240

# Mean along rows
rray_mean(x, 1L)
#>      [,1] [,2]
#> [1,]    3    8

y <- array(c(TRUE, TRUE, FALSE, TRUE), c(2L, 2L))

rray_all(y, 1L)
#>      [,1]  [,2]
#> [1,] TRUE FALSE
rray_any(y, 1L)
#>      [,1] [,2]
#> [1,] TRUE TRUE

# Maximum and minimum along columns
rray_max(x, 2L)
#>      [,1]
#> [1,]    6
#> [2,]    7
#> [3,]    8
#> [4,]    9
#> [5,]   10
rray_min(x, 2L)
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
#> [4,]    4
#> [5,]    5
```
