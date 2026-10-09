# Unstack an array

`rray_unstack()` splits an array along `axis` and then removes that
`axis` from each of the resulting arrays. The result is a list of arrays
that have a dimensionality 1 less than `x` itself.

Using [`rray_stack()`](https://rray.r-lib.org/reference/rray_stack.md)
along the same `axis` reconstructs `x`.

## Usage

``` r
rray_unstack(x, axis)
```

## Arguments

- x:

  An array.

- axis:

  A single integer representing the axis to unstack along.

## Value

A list of arrays with the same dimensions as `x`, except that `axis` has
been removed.

## Details

Names along `axis` become the names of the returned list. Names on the
surviving axes are carried over to their new locations.

Because the dimensionality is reduced by 1, `x` must have a
dimensionality of at least 2 to begin with.

## See also

[`rray_stack()`](https://rray.r-lib.org/reference/rray_stack.md)

## Examples

``` r
x <- array(1:6, c(2, 3))

# One array per row
rray_unstack(x, 1)
#> [[1]]
#> [1] 1 3 5
#> 
#> [[2]]
#> [1] 2 4 6
#> 

# One array per column
rray_unstack(x, 2)
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 3 4
#> 
#> [[3]]
#> [1] 5 6
#> 

# Names along the axis become list names
y <- array(1:4, c(2, 2), dimnames = list(c("a", "b"), c("x", "y")))
rray_unstack(y, 2)
#> $x
#> a b 
#> 1 2 
#> 
#> $y
#> a b 
#> 3 4 
#> 

# Unstacking and stacking along the same axis are inverses
rray_stack(!!!rray_unstack(x, 2), .axis = 2)
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
```
