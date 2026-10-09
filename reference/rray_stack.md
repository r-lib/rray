# Stack arrays along a new axis

`rray_stack()` joins arrays along a new axis inserted at `.axis`. All
preexisting axes are broadcast to their common dimension.

The new axis has a dimension equal to the number of arrays you are
stacking.

## Usage

``` r
rray_stack(..., .axis)
```

## Arguments

- ...:

  Arrays to stack.

- .axis:

  A single integer representing the axis in the *result* to stack along.
  Must be between 1 and one greater than the maximum dimensionality of
  the input arrays. For example, you can stack 2D matrices along axis 3,
  but not axis 4.

## Value

An array with the following dimensions:

- Along `.axis`, the number of inputs.

- Along all other axes, the common dimensions of the inputs taken via
  broadcasting.

## Details

Names of `...` become the names of the new axis. Existing axis names are
otherwise carried over.

## See also

[`rray_unstack()`](https://rray.r-lib.org/reference/rray_unstack.md),
[`rray_combine()`](https://rray.r-lib.org/reference/rray_combine.md)

## Examples

``` r
x <- array(1:12, c(3, 4))
y <- array(13:24, c(3, 4))

# (3, 4) -> (2, 3, 4)
rray_dimensions(rray_stack(x, y, .axis = 1))
#> [1] 2 3 4

# (3, 4) -> (3, 4, 2)
rray_stack(x, y, .axis = 3)
#> , , 1
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]    1    4    7   10
#> [2,]    2    5    8   11
#> [3,]    3    6    9   12
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3] [,4]
#> [1,]   13   16   19   22
#> [2,]   14   17   20   23
#> [3,]   15   18   21   24
#> 

# Existing axes are broadcast
# (3, 1) and (3, 4) -> (2, 3, 4)
a <- array(1:3, c(3, 1))
b <- array(1:12, c(3, 4))
rray_dimensions(rray_stack(a, b, .axis = 1))
#> [1] 2 3 4

# Names of `...` name the new axis
rray_stack(first = 1:2, second = 3:4, .axis = 2)
#>      first second
#> [1,]     1      3
#> [2,]     2      4
```
