# Logical operations

- `rray_and()` tests whether both `x` and `y` are `TRUE`.

- `rray_or()` tests whether at least one of `x` and `y` is `TRUE`.

- `rray_xor()` tests whether exactly one of `x` and `y` is `TRUE`.

## Usage

``` r
rray_and(x, y)

rray_or(x, y)

rray_xor(x, y)
```

## Arguments

- x, y:

  Logical arrays.

## Value

A logical array with the common dimensions of `x` and `y`.

## Details

The arrays are broadcast to common dimensions before the operation is
applied.

Missing values are handled the same way as `&`, `|`, and
[`xor()`](https://rdrr.io/r/base/Logic.html).

## Examples

``` r
x <- array(c(TRUE, FALSE, TRUE), c(3L, 1L))
y <- array(c(TRUE, FALSE), c(1L, 2L))

rray_and(x, y)
#>       [,1]  [,2]
#> [1,]  TRUE FALSE
#> [2,] FALSE FALSE
#> [3,]  TRUE FALSE
rray_or(x, y)
#>      [,1]  [,2]
#> [1,] TRUE  TRUE
#> [2,] TRUE FALSE
#> [3,] TRUE  TRUE
rray_xor(x, y)
#>       [,1]  [,2]
#> [1,] FALSE  TRUE
#> [2,]  TRUE FALSE
#> [3,] FALSE  TRUE

rray_and(NA, FALSE)
#> [1] FALSE
rray_or(NA, TRUE)
#> [1] TRUE
rray_xor(NA, TRUE)
#> [1] NA
```
