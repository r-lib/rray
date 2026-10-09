# Split an array along an axis

`rray_split()` divides `x` into contiguous arrays along an `axis`.

Using
[`rray_combine()`](https://rray.r-lib.org/reference/rray_combine.md)
along the same `axis` reconstructs `x`.

## Usage

``` r
rray_split(x, ..., axis, dimensions)
```

## Arguments

- x:

  An array.

- ...:

  These dots are for future extensions and must be empty.

- axis:

  A single integer representing the axis to split on.

- dimensions:

  One of:

  - A single positive integer, used as the dimension of every array. It
    must evenly divide the dimension of `x` along `axis`.

  - A vector of positive (or zero) integers, giving the dimension of
    each array directly. They must sum to the dimension of `x` along
    `axis`.

## Value

A list of arrays each with the same dimensions as `x`, except along
`axis`.

## Examples

``` r
x <- array(1:12, c(6, 2))

# Three arrays of two rows
rray_split(x, axis = 1, dimensions = 2)
#> [[1]]
#>      [,1] [,2]
#> [1,]    1    7
#> [2,]    2    8
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    3    9
#> [2,]    4   10
#> 
#> [[3]]
#>      [,1] [,2]
#> [1,]    5   11
#> [2,]    6   12
#> 

# Arrays of one and five rows
rray_split(x, axis = 1, dimensions = c(1, 5))
#> [[1]]
#>      [,1] [,2]
#> [1,]    1    7
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    2    8
#> [2,]    3    9
#> [3,]    4   10
#> [4,]    5   11
#> [5,]    6   12
#> 

# One array per column
rray_split(x, axis = 2, dimensions = 1)
#> [[1]]
#>      [,1]
#> [1,]    1
#> [2,]    2
#> [3,]    3
#> [4,]    4
#> [5,]    5
#> [6,]    6
#> 
#> [[2]]
#>      [,1]
#> [1,]    7
#> [2,]    8
#> [3,]    9
#> [4,]   10
#> [5,]   11
#> [6,]   12
#> 

# Arrays of dimension zero are allowed in the explicit form
rray_split(x, axis = 2, dimensions = c(0, 2))
#> [[1]]
#>     
#> [1,]
#> [2,]
#> [3,]
#> [4,]
#> [5,]
#> [6,]
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    1    7
#> [2,]    2    8
#> [3,]    3    9
#> [4,]    4   10
#> [5,]    5   11
#> [6,]    6   12
#> 

# Splitting and combining along the same axis are inverses
rray_combine(!!!rray_split(x, axis = 1, dimensions = 3), .axis = 1)
#>      [,1] [,2]
#> [1,]    1    7
#> [2,]    2    8
#> [3,]    3    9
#> [4,]    4   10
#> [5,]    5   11
#> [6,]    6   12
```
