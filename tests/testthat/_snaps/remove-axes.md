# errors when a selected axis does not have dimension 1

    Code
      rray_remove_axes(array(1:2, c(2L, 1L)), 1L)
    Condition
      Error in `rray_remove_axes()`:
      ! Can't remove axis 1 of `x` because it has dimension 2, not 1.
    Code
      rray_remove_axes(array(integer(), c(0L, 1L)), 1L)
    Condition
      Error in `rray_remove_axes()`:
      ! Can't remove axis 1 of `x` because it has dimension 0, not 1.

# errors when every axis is removed

    Code
      rray_remove_axes(array(1L, c(1L, 1L, 1L)), c(1L, 2L, 3L))
    Condition
      Error in `rray_remove_axes()`:
      ! `axes` can't remove every axis.
    Code
      rray_remove_axes(array(1L, 1L), 1L)
    Condition
      Error in `rray_remove_axes()`:
      ! `axes` can't remove every axis.

# errors on invalid axes

    Code
      rray_remove_axes(x, 3L)
    Condition
      Error in `rray_remove_axes()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.
    Code
      rray_remove_axes(x, 0L)
    Condition
      Error in `rray_remove_axes()`:
      ! `axes` must contain values greater than or equal to 1, not 0.
    Code
      rray_remove_axes(x, NA_integer_)
    Condition
      Error in `rray_remove_axes()`:
      ! `axes` must not contain missing values.
    Code
      rray_remove_axes(x, c(1L, 1L))
    Condition
      Error in `rray_remove_axes()`:
      ! `axes` must be in strictly increasing order.
    Code
      rray_remove_axes(x, c(2L, 1L))
    Condition
      Error in `rray_remove_axes()`:
      ! `axes` must be in strictly increasing order.
    Code
      rray_remove_axes(x, structure(1L, foo = "bar"))
    Condition
      Error in `rray_remove_axes()`:
      ! `axes` can't have attributes.
    Code
      rray_remove_axes(x, 1.5)
    Condition
      Error in `rray_remove_axes()`:
      ! `axes` must contain whole numbers that fit in an integer. Problem at location 1.
    Code
      rray_remove_axes(x, "x")
    Condition
      Error in `rray_remove_axes()`:
      ! `axes` must be an integer or double vector, not the string "x".

# errors on non-array input

    Code
      rray_remove_axes(NULL, 1L)
    Condition
      Error in `rray_remove_axes()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_remove_axes(mean, 1L)
    Condition
      Error in `rray_remove_axes()`:
      ! `x` must be an array, not a function.

# errors on classed input

    Code
      rray_remove_axes(x, 1L)
    Condition
      Error in `rray_remove_axes()`:
      ! `x` must be a bare array, not a <foo> object.

