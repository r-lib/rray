# errors when a selected axis does not have dimension 1

    Code
      rray_squeeze(array(1:2, c(2L, 1L)), 1L)
    Condition
      Error in `rray_squeeze()`:
      ! Can't squeeze axis 1 of `x` because it has dimension 2, not 1.
    Code
      rray_squeeze(array(integer(), c(0L, 1L)), 1L)
    Condition
      Error in `rray_squeeze()`:
      ! Can't squeeze axis 1 of `x` because it has dimension 0, not 1.

# errors on invalid axes

    Code
      rray_squeeze(x, 3L)
    Condition
      Error in `rray_squeeze()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.
    Code
      rray_squeeze(x, 0L)
    Condition
      Error in `rray_squeeze()`:
      ! `axes` must contain values greater than or equal to 1, not 0.
    Code
      rray_squeeze(x, NA_integer_)
    Condition
      Error in `rray_squeeze()`:
      ! `axes` must not contain missing values.
    Code
      rray_squeeze(x, c(1L, 1L))
    Condition
      Error in `rray_squeeze()`:
      ! `axes` must be in strictly increasing order.
    Code
      rray_squeeze(x, c(2L, 1L))
    Condition
      Error in `rray_squeeze()`:
      ! `axes` must be in strictly increasing order.
    Code
      rray_squeeze(x, structure(1L, foo = "bar"))
    Condition
      Error in `rray_squeeze()`:
      ! `axes` can't have attributes.
    Code
      rray_squeeze(x, 1.5)
    Condition
      Error:
      ! Can't convert from `axes` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      rray_squeeze(x, "x")
    Condition
      Error:
      ! Can't convert `axes` <character> to <integer>.

# errors on non-array input

    Code
      rray_squeeze(NULL, 1L)
    Condition
      Error in `rray_squeeze()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_squeeze(mean, 1L)
    Condition
      Error in `rray_squeeze()`:
      ! `x` must be an array, not a function.

# errors on classed input

    Code
      rray_squeeze(x, 1L)
    Condition
      Error in `rray_squeeze()`:
      ! `x` must be a bare array, not a <foo> object.

