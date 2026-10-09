# Compare arrays

- `rray_greater_than()` tests whether `x` is greater than `y`.

- `rray_greater_than_or_equal()` tests whether `x` is greater than or
  equal to `y`.

- `rray_less_than()` tests whether `x` is less than `y`.

- `rray_less_than_or_equal()` tests whether `x` is less than or equal to
  `y`.

## Usage

``` r
rray_greater_than(x, y)

rray_greater_than_or_equal(x, y)

rray_less_than(x, y)

rray_less_than_or_equal(x, y)
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
x <- array(1:6, c(3L, 2L))

rray_greater_than(x, 3L)
#>       [,1] [,2]
#> [1,] FALSE TRUE
#> [2,] FALSE TRUE
#> [3,] FALSE TRUE
rray_less_than_or_equal(x, array(c(2L, 5L), c(1L, 2L)))
#>       [,1]  [,2]
#> [1,]  TRUE  TRUE
#> [2,]  TRUE  TRUE
#> [3,] FALSE FALSE
```
