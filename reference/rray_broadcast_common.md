# Broadcast arrays to common dimensions

`rray_broadcast_common()` broadcasts every array to the common
dimensions found by
[`rray_dimensions_common()`](https://rray.r-lib.org/reference/rray_dimensions_common.md).

## Usage

``` r
rray_broadcast_common(..., .dimensions = NULL)
```

## Arguments

- ...:

  Arrays to broadcast.

- .dimensions:

  If provided, an integer vector of dimensions to broadcast to, rather
  than computing common dimensions from `...`.

## Value

A list of arrays, all with the common dimensions.

## Examples

``` r
rray_broadcast_common(array(1:3, c(3L, 1L)), array(1:2, c(1L, 2L)))
#> [[1]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    2    2
#> [3,]    3    3
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    1    2
#> [2,]    1    2
#> [3,]    1    2
#> 

# Names of `...` are kept
rray_broadcast_common(x = 1:3, y = 1L)
#> $x
#> [1] 1 2 3
#> 
#> $y
#> [1] 1 1 1
#> 

# `.dimensions` overrides the common dimensions
rray_broadcast_common(1:3, .dimensions = c(3L, 2L))
#> [[1]]
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    2    2
#> [3,]    3    3
#> 
```
