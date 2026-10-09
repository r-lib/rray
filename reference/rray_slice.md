# Slice an array

`rray_slice()` selects positions along each axis of `x`. The result
always has the same dimensionality as `x`.

`rray_slice_axis()` is `rray_slice()` restricted to a single axis, which
can be more ergonomic to write. `rray_slice_rows()` and
`rray_slice_columns()` are shortcuts for `axis = 1` and `axis = 2`.

The `_assign` versions replace the selected positions with `value` and
return a modified copy of `x`.

## Usage

``` r
rray_slice(x, ...)

rray_slice_axis(x, i, ..., axis)

rray_slice_rows(x, i)

rray_slice_columns(x, i)

rray_slice_assign(x, ..., value)

rray_slice_assign_axis(x, i, ..., axis, value)

rray_slice_assign_rows(x, i, value)

rray_slice_assign_columns(x, i, value)
```

## Arguments

- x:

  An array.

- ...:

  One unnamed subscript for each axis of `x`, in axis order. Each
  subscript is one of:

  - `TRUE`, to select the whole axis.

  - A logical vector the size of the axis. `TRUE` selects a position.

  - An integer or double vector of locations. Negative values drop
    positions, zero is ignored, and duplicates repeat positions.

  - A character vector of names, matched against the names of the axis.
    The first match is used when names are duplicated.

  - `NULL`, to select nothing.

  For `rray_slice_axis()` and `rray_slice_assign_axis()`, these dots
  must be empty.

- i:

  A subscript for the selected axis. It takes any of the forms allowed
  in `...`.

- axis:

  A single integer. The axis to slice along.

- value:

  An array to assign to the selected positions. It is cast to the type
  of `x`, then broadcast to the dimensions of the selection.

## Value

- `rray_slice()`, `rray_slice_axis()`, `rray_slice_rows()`, and
  `rray_slice_columns()` return an array with the same type and
  dimensionality as `x`. The dimension of each axis is the number of
  positions selected on it. The names of each axis are selected along
  with the values.

- The `_assign` versions return `x` with the selected positions
  replaced. The type, dimensions, and names of `x` are kept.

## Details

`NA` in a subscript gives missing values in the result. For raw arrays
the missing value is `as.raw(0)`, and for list arrays it is `NULL`. If
the axis has names, the name of a missing value is `""`. For assignment,
`NA` in a subscript leaves `x` unchanged at that position but uses one
element of `value`.

Unlike `[`, an empty argument does not select a whole axis. Use `TRUE`
instead, so that every axis is provided explicitly:

    x <- array(1:24, c(2L, 3L, 4L))

    # Base R
    x[1, , , drop = FALSE]

    # rray
    rray_slice(x, 1, TRUE, TRUE)

    # Or, more simply
    rray_slice_axis(x, 1, axis = 1)
    rray_slice_rows(x, 1)

## Examples

``` r
x <- array(1:24, c(2L, 3L, 4L))

# The first row
rray_slice(x, 1, TRUE, TRUE)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    7    9   11
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   13   15   17
#> 
#> , , 4
#> 
#>      [,1] [,2] [,3]
#> [1,]   19   21   23
#> 

# Reorder and repeat positions
rray_slice(x, c(2, 1, 2), 1, 4)
#> , , 1
#> 
#>      [,1]
#> [1,]   20
#> [2,]   19
#> [3,]   20
#> 

# Drop positions with negative locations
rray_slice(x, TRUE, -2, -(1:2))
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]   13   17
#> [2,]   14   18
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]   19   23
#> [2,]   20   24
#> 

# Select by name
y <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("c", "d", "e")))
rray_slice(y, "b", c("e", "c"))
#>   e c
#> b 6 2

# Subscripts can be spliced into `...`
subscripts <- rep(list(TRUE), rray_dimensionality(x))
subscripts[[3]] <- c(4, 1)
rray_slice(x, !!!subscripts)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]   19   21   23
#> [2,]   20   22   24
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 

# Slice a single axis
rray_slice_axis(x, c(4, 1), axis = 3)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]   19   21   23
#> [2,]   20   22   24
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
#> 
rray_slice_rows(y, c("b", "a"))
#>   c d e
#> b 2 4 6
#> a 1 3 5
rray_slice_columns(x, -2)
#> , , 1
#> 
#>      [,1] [,2]
#> [1,]    1    5
#> [2,]    2    6
#> 
#> , , 2
#> 
#>      [,1] [,2]
#> [1,]    7   11
#> [2,]    8   12
#> 
#> , , 3
#> 
#>      [,1] [,2]
#> [1,]   13   17
#> [2,]   14   18
#> 
#> , , 4
#> 
#>      [,1] [,2]
#> [1,]   19   23
#> [2,]   20   24
#> 

# Assign one value to every first row
rray_slice_assign(x, 1, TRUE, TRUE, value = 0L)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    2    4    6
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    8   10   12
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]   14   16   18
#> 
#> , , 4
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]   20   22   24
#> 
rray_slice_assign_rows(x, 1, 0L)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    2    4    6
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]    8   10   12
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]   14   16   18
#> 
#> , , 4
#> 
#>      [,1] [,2] [,3]
#> [1,]    0    0    0
#> [2,]   20   22   24
#> 

# Or broadcast `value` to the dimensions of the selection
value <- array(c(100L, 200L), c(1L, 2L))
rray_slice_assign(x, TRUE, c(1, 3), 1, value = value)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]  100    3  200
#> [2,]  100    4  200
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

value <- array(c(100L, 200L), c(2L, 1L))
rray_slice_assign_axis(x, 3, axis = 2, value = value)
#> , , 1
#> 
#>      [,1] [,2] [,3]
#> [1,]    1    3  100
#> [2,]    2    4  200
#> 
#> , , 2
#> 
#>      [,1] [,2] [,3]
#> [1,]    7    9  100
#> [2,]    8   10  200
#> 
#> , , 3
#> 
#>      [,1] [,2] [,3]
#> [1,]   13   15  100
#> [2,]   14   16  200
#> 
#> , , 4
#> 
#>      [,1] [,2] [,3]
#> [1,]   19   21  100
#> [2,]   20   22  200
#> 
```
