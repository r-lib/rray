# Equality

- `rray_equal()` tests whether `x` is equal to `y`.

- `rray_not_equal()` tests whether `x` is not equal to `y`.

## Usage

``` r
rray_equal(x, y)

rray_not_equal(x, y)
```

## Arguments

- x:

  An array.

- y:

  An array.

## Value

A logical array with the common dimensions of `x` and `y`.

## Details

The arrays are broadcast to common dimensions before they are compared.

## Examples

``` r
x <- array(1:2, c(2L, 1L))

rray_equal(x, 1L)
#>       [,1]
#> [1,]  TRUE
#> [2,] FALSE
rray_not_equal(x, array(c(1L, 3L), c(1L, 2L)))
#>       [,1] [,2]
#> [1,] FALSE TRUE
#> [2,]  TRUE TRUE
```
