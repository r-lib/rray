# Set the dimensions of an array

`rray_set_dimensions()` sets the dimensions of `x` to a new set of
dimensions without changing the total number of elements. Unlike
[`rray_broadcast()`](https://rray.r-lib.org/reference/rray_broadcast.md),
which repeats elements to fill new dimensions, `rray_set_dimensions()`
simply reinterprets the existing elements under new dimensions without
changing its size.

## Usage

``` r
rray_set_dimensions(x, dimensions)
```

## Arguments

- x:

  An array.

- dimensions:

  An integer vector of new dimensions.

## Value

An array with new `dimensions` but the same size as `x`.

## Examples

``` r
x <- 1:6

# Set the dimensions to turn a vector into a matrix
rray_set_dimensions(x, c(2L, 3L))
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6

# Set the dimensions to turn a vector into a 3D array
rray_set_dimensions(x, c(3L, 2L, 1L))
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    1    4
#> [2,]    2    5
#> [3,]    3    6
#> 

# Setting dimensions can't change the size
try(rray_set_dimensions(x, c(6L, 2L)))
#> Error in rray_set_dimensions(x, c(6L, 2L)) : 
#>   Can't set these dimensions. Can't change from a size of 6 to a size of 12.
```
