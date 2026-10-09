# Array arithmetic

- `rray_add()` adds two arrays elementwise.

- `rray_subtract()` subtracts two arrays elementwise.

- `rray_multiply()` multiplies two arrays elementwise.

- `rray_divide()` divides two arrays elementwise.

- `rray_exponentiate()` raises the elements of one array to the power of
  the elements of another.

## Usage

``` r
rray_add(x, y)

rray_subtract(x, y)

rray_multiply(x, y)

rray_divide(x, y)

rray_exponentiate(x, y)
```

## Arguments

- x:

  An array.

- y:

  An array.

## Value

An array with the common dimensions of `x` and `y`.

## Details

The arrays are broadcast to common dimensions first, so they do not have
to be the same shape.

If the result of an integer operation would overflow, an error is
thrown.

## Casting

Certain inputs are upcast, changing the return type:

- `rray_add()`: logicals are cast to integer.

- `rray_subtract()`: logicals are cast to integer.

- `rray_multiply()`: logicals are cast to integer.

- `rray_divide()`: logicals and integers are cast to double.

- `rray_exponentiate()`: logicals and integers are cast to double.

## Examples

``` r
x <- array(1:6, c(3L, 2L))

rray_add(x, 1L)
#>      [,1] [,2]
#> [1,]    2    5
#> [2,]    3    6
#> [3,]    4    7
rray_subtract(x, 1L)
#>      [,1] [,2]
#> [1,]    0    3
#> [2,]    1    4
#> [3,]    2    5
rray_multiply(x, 2L)
#>      [,1] [,2]
#> [1,]    2    8
#> [2,]    4   10
#> [3,]    6   12
rray_divide(x, 2L)
#>      [,1] [,2]
#> [1,]  0.5  2.0
#> [2,]  1.0  2.5
#> [3,]  1.5  3.0
rray_exponentiate(x, 2L)
#>      [,1] [,2]
#> [1,]    1   16
#> [2,]    4   25
#> [3,]    9   36

# Adding 10 to column 1 and 20 to column 2 via broadcasting
rray_add(x, array(c(10L, 20L), c(1L, 2L)))
#>      [,1] [,2]
#> [1,]   11   24
#> [2,]   12   25
#> [3,]   13   26

# Scaling column 1 by 2 and column 2 by 3 the same way
rray_multiply(x, array(c(2L, 3L), c(1L, 2L)))
#>      [,1] [,2]
#> [1,]    2   12
#> [2,]    4   15
#> [3,]    6   18

# Names are collected from both inputs, one axis at a time
rows <- array(1:3, c(3L, 1L), dimnames = list(c("r1", "r2", "r3")))
cols <- array(c(10L, 20L), c(1L, 2L), dimnames = list(NULL, c("c1", "c2")))
rray_add(rows, cols)
#>    c1 c2
#> r1 11 21
#> r2 12 22
#> r3 13 23

# A broadcast axis loses its names, because they no longer describe the
# result. `only` names a single row, but the result has three.
cols <- array(c(10L, 20L), c(1L, 2L), dimnames = list("only", c("c1", "c2")))
rray_add(x, cols)
#>      c1 c2
#> [1,] 11 24
#> [2,] 12 25
#> [3,] 13 26
```
