# Insert array axes

`rray_insert_axes()` inserts new axes with a dimension of 1.

## Usage

``` r
rray_insert_axes(x, axes)
```

## Arguments

- x:

  An array.

- axes:

  An integer vector referring to axes in the *result* that are newly
  inserted.

## Value

An array with new axes of dimension 1 at `axes`.

## Details

`axes` refer to axes of the result, not axes of `x`. Inserting 2 axes
into an array with a dimensionality of 3 gives a result with a
dimensionality of 5 (2 + 3), so `axes` can be any value between 1 and 5.

To work out the result, write out the axes of the result and mark the
ones listed in `axes` with 1. Then use the dimensions of `x` to fill in
the rest in order.

    x       (2, 3, 4)
    axes    2

    result  (x, 1, x, x)
          = (2, 1, 3, 4)

This allows you to insert two axes side by side:

    x       (2, 3, 4)
    axes    c(2, 3)

    result  (x, 1, 1, x, x)
          = (2, 1, 1, 3, 4)

Inserted axes have no names. The axes of `x` keep their names and carry
them to their new locations.

## See also

[`rray_remove_axes()`](https://rray.r-lib.org/reference/rray_remove_axes.md)

## Examples

``` r
x <- array(1:24, c(2, 3, 4))

# Insert one axis in the middle
# (2, 3, 4) -> (2, 1, 3, 4)
rray_dimensions(rray_insert_axes(x, 2))
#> [1] 2 1 3 4

# Insert at the front and at the back
# (2, 3, 4) -> (1, 2, 3, 4, 1)
rray_dimensions(rray_insert_axes(x, c(1, 5)))
#> [1] 1 2 3 4 1

# Two new axes side by side
# (2, 3, 4) -> (2, 1, 1, 3, 4)
rray_dimensions(rray_insert_axes(x, c(2, 3)))
#> [1] 2 1 1 3 4

# Inserting no axes returns `x` unchanged
rray_insert_axes(x, integer())
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    7    9   11
#> [2,]    8   10   12
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   13   15   17
#> [2,]   14   16   18
#> 
#> , , 4
#> 
#>      [,1] [,2] [,3]
#> [1,]   19   21   23
#> [2,]   20   22   24
#> 

# `rray_remove_axes()` undoes an insertion
rray_remove_axes(rray_insert_axes(x, 2), 2)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    7    9   11
#> [2,]    8   10   12
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   13   15   17
#> [2,]   14   16   18
#> 
#> , , 4
#> 
#>      [,1] [,2] [,3]
#> [1,]   19   21   23
#> [2,]   20   22   24
#> 
```
