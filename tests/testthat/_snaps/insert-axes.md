# errors on invalid axes

    Code
      rray_insert_axes(x, 4L)
    Condition
      Error in `rray_insert_axes()`:
      ! `axes` must contain values less than or equal to the dimensionality of 3, not 4.
    Code
      rray_insert_axes(x, 0L)
    Condition
      Error in `rray_insert_axes()`:
      ! `axes` must contain values greater than or equal to 1, not 0.
    Code
      rray_insert_axes(x, NA_integer_)
    Condition
      Error in `rray_insert_axes()`:
      ! `axes` must not contain missing values.
    Code
      rray_insert_axes(x, c(1L, 1L))
    Condition
      Error in `rray_insert_axes()`:
      ! `axes` must be in strictly increasing order.
    Code
      rray_insert_axes(x, c(2L, 1L))
    Condition
      Error in `rray_insert_axes()`:
      ! `axes` must be in strictly increasing order.
    Code
      rray_insert_axes(x, structure(1L, foo = "bar"))
    Condition
      Error in `rray_insert_axes()`:
      ! `axes` can't have attributes.
    Code
      rray_insert_axes(x, 1.5)
    Condition
      Error:
      ! Can't convert from `axes` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      rray_insert_axes(x, "x")
    Condition
      Error:
      ! Can't convert `axes` <character> to <integer>.

# errors on dimensionality upper bound

    Code
      rray_insert_axes(array(1, dim = rep(1L, 64)), 1L)
    Condition
      Error in `rray_insert_axes()`:
      ! rray can't support arrays with a dimensionality greater than 64. A dimensionality of 65 was requested.

# errors on non-array input

    Code
      rray_insert_axes(NULL, 1L)
    Condition
      Error in `rray_insert_axes()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_insert_axes(mean, 1L)
    Condition
      Error in `rray_insert_axes()`:
      ! `x` must be an array, not a function.

# errors on classed input

    Code
      rray_insert_axes(x, 1L)
    Condition
      Error in `rray_insert_axes()`:
      ! `x` must be a bare array, not a <foo> object.

