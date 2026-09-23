# errors above the maximum result dimensionality

    Code
      rray_index(42L, index)
    Condition
      Error in `rray_index()`:
      ! rray can't support arrays with a dimensionality greater than 64. A dimensionality of 65 was requested.

# requires one coordinate per source axis

    Code
      rray_index(x)
    Condition
      Error in `rray_index()`:
      ! Must supply exactly 2 coordinate arrays to `...`, not 0.
    Code
      rray_index(x, 1L)
    Condition
      Error in `rray_index()`:
      ! Must supply exactly 2 coordinate arrays to `...`, not 1.
    Code
      rray_index(x, 1L, 1L, 1L)
    Condition
      Error in `rray_index()`:
      ! Must supply exactly 2 coordinate arrays to `...`, not 3.

# requires unnamed coordinates

    Code
      rray_index(x, rows = 1L, 1L)
    Condition
      Error in `rray_index()`:
      ! All elements of `...` must be unnamed.
    Code
      rray_index(x, 1L, columns = 1L)
    Condition
      Error in `rray_index()`:
      ! All elements of `...` must be unnamed.

# validates every coordinate before broadcasting

    Code
      rray_index(x, integer(), 0L)
    Condition
      Error in `rray_index()`:
      ! `..2` must only contain positive values or missing values.
    Code
      rray_index(x, integer(), 4L)
    Condition
      Error in `rray_index()`:
      ! `..2` must not contain values greater than 3.
    Code
      rray_index(x, integer(), NULL)
    Condition
      Error in `rray_index()`:
      ! `..2` must be an integer array, not `NULL`.
    Code
      rray_index(x, 1L, factor("a"))
    Condition
      Error in `rray_index()`:
      ! `..2` must be a bare array, not a <factor> object.

# errors on incompatible coordinate dimensions

    Code
      rray_index(x, rows, columns)
    Condition
      Error in `rray_index()`:
      ! Can't find common dimensions at axis 1. `..1` has dimension 2 and `..2` has dimension 3.

# combines zero dimensions by broadcasting rules

    Code
      rray_index(x, array(integer(), c(0L, 2L)), array(1L, c(3L, 2L)))
    Condition
      Error in `rray_index()`:
      ! Can't find common dimensions at axis 1. `..1` has dimension 0 and `..2` has dimension 3.

# errors on unsupported `x` inputs

    Code
      rray_index(NULL, 1L)
    Condition
      Error in `rray_index()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_index(mean, 1L)
    Condition
      Error in `rray_index()`:
      ! `x` must be an array, not a function.
    Code
      rray_index(structure(1:2, class = "foo"), 1L)
    Condition
      Error in `rray_index()`:
      ! `x` must be a bare array, not a <foo> object.

# `rray_as_index_array()` requires bare integer input

    Code
      rray_as_index_array(NULL, 2L)
    Condition
      Error in `rray_as_index_array()`:
      ! `x` must be an integer array, not `NULL`.
    Code
      rray_as_index_array(c(TRUE, FALSE), 2L)
    Condition
      Error in `rray_as_index_array()`:
      ! `x` must be an integer array, not a logical vector.
    Code
      rray_as_index_array(c(1, 2), 2L)
    Condition
      Error in `rray_as_index_array()`:
      ! `x` must be an integer array, not a double vector.
    Code
      rray_as_index_array(c("a", "b"), 2L)
    Condition
      Error in `rray_as_index_array()`:
      ! `x` must be an integer array, not a character vector.
    Code
      rray_as_index_array(factor(c("a", "b")), 2L)
    Condition
      Error in `rray_as_index_array()`:
      ! `x` must be a bare array, not a <factor> object.
    Code
      rray_as_index_array(structure(1:2, class = "foo"), 2L)
    Condition
      Error in `rray_as_index_array()`:
      ! `x` must be a bare array, not a <foo> object.

# `rray_as_index_array()` checks coordinates

    Code
      rray_as_index_array(c(0L, 1L), 2L)
    Condition
      Error in `rray_as_index_array()`:
      ! `x` must only contain positive values or missing values.
    Code
      rray_as_index_array(c(-1L, 1L), 2L)
    Condition
      Error in `rray_as_index_array()`:
      ! `x` must only contain positive values or missing values.
    Code
      rray_as_index_array(c(1L, 3L), 2L)
    Condition
      Error in `rray_as_index_array()`:
      ! `x` must not contain values greater than 2.
    Code
      rray_as_index_array(1L, 0L)
    Condition
      Error in `rray_as_index_array()`:
      ! `x` must not contain values greater than 0.

# `rray_as_index_array()` checks `dimension`

    Code
      rray_as_index_array(1L, NULL)
    Condition
      Error in `rray_as_index_array()`:
      ! `dimension` must be a single integer, not length 0.
    Code
      rray_as_index_array(1L, integer())
    Condition
      Error in `rray_as_index_array()`:
      ! `dimension` must be a single integer, not length 0.
    Code
      rray_as_index_array(1L, c(1L, 2L))
    Condition
      Error in `rray_as_index_array()`:
      ! `dimension` must be a single integer, not length 2.
    Code
      rray_as_index_array(1L, NA_integer_)
    Condition
      Error in `rray_as_index_array()`:
      ! `dimension` must not be missing.
    Code
      rray_as_index_array(1L, -1L)
    Condition
      Error in `rray_as_index_array()`:
      ! `dimension` must not be negative.
    Code
      rray_as_index_array(1L, 1.5)
    Condition
      Error:
      ! Can't convert from `dimension` <double> to <integer> due to loss of precision.
      * Locations: 1

