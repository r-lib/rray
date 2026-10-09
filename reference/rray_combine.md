# Combine arrays along an existing axis

`rray_combine()` joins one or more arrays along `.axis`. Dimensions on
every other axis are broadcast to common dimensions.

## Usage

``` r
rray_combine(..., .axis)
```

## Arguments

- ...:

  Arrays to combine.

- .axis:

  A single integer between 1 and the greatest input dimensionality.

## Value

An array with the following dimensions:

- Along `.axis`, the input dimensions are added together.

- Along all other axes, the common dimensions are taken via
  broadcasting.

## Examples

``` r
x <- array(1:6, c(2, 3))
y <- array(7:12, c(2, 3))

rray_combine(x, y, .axis = 1)
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> [3,]    7    9   11
#> [4,]    8   10   12
rray_combine(x, y, .axis = 2)
#>      [,1] [,2] [,3] [,4] [,5] [,6]
#> [1,]    1    3    5    7    9   11
#> [2,]    2    4    6    8   10   12

# Missing trailing axes have an implicit dimension of 1
rray_combine(1:2, array(3:8, c(2, 3)), .axis = 2)
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    3    5    7
#> [2,]    2    4    6    8
```
