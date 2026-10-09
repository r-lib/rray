# Broadcast an array to new dimensions

`rray_broadcast()` broadcasts an array to a new set of dimensions using
tidyverse recycling rules. Each dimension of `x` must either match the
corresponding target dimension or be 1, in which case it is repeated to
fill the target.

Dimensionality can be expanded by supplying `dimensions` with greater
dimensionality than `x` has. For example, a 2x3 array can be broadcast
to a 2x3x4 array.

## Usage

``` r
rray_broadcast(x, dimensions)
```

## Arguments

- x:

  An array.

- dimensions:

  An integer vector of target dimensions.

## Value

An array with dimensions of `dimensions`.

## Examples

``` r
# Broadcast a vector to a matrix
rray_broadcast(1:3, c(3L, 2L))
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    2    2
#> [3,]    3    3

# Broadcast a row to fill a matrix
rray_broadcast(array(1:2, c(1L, 2L)), c(3L, 2L))
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    1    2
#> [3,]    1    2

# Add a new dimension
rray_broadcast(array(1:6, c(2L, 3L)), c(2L, 3L, 4L))
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 
#> , , 4
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 
```
