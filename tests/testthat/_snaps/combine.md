# validates inputs and axis

    Code
      rray_combine(.axis = 1L)
    Condition
      Error in `rray_combine()`:
      ! Must supply at least one array to `...`.
    Code
      rray_combine(x)
    Condition
      Error in `rray_combine()`:
      ! argument ".axis" is missing, with no default
    Code
      rray_combine(x, .axis = integer())
    Condition
      Error in `rray_combine()`:
      ! `.axis` must be a single integer, not length 0.
    Code
      rray_combine(x, .axis = c(1L, 2L))
    Condition
      Error in `rray_combine()`:
      ! `.axis` must be a single integer, not length 2.
    Code
      rray_combine(x, .axis = NA_integer_)
    Condition
      Error in `rray_combine()`:
      ! `.axis` must not be missing.
    Code
      rray_combine(x, .axis = structure(1L, foo = "bar"))
    Condition
      Error in `rray_combine()`:
      ! `.axis` can't have attributes.
    Code
      rray_combine(x, .axis = 1.5)
    Condition
      Error:
      ! Can't convert from `.axis` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      rray_combine(x, .axis = 0L)
    Condition
      Error in `rray_combine()`:
      ! `.axis` must be greater than or equal to 1, not 0.
    Code
      rray_combine(x, .axis = 3L)
    Condition
      Error in `rray_combine()`:
      ! `.axis` must be less than or equal to the dimensionality of 2, not 3.

# rejects incompatible inputs

    Code
      rray_combine(array(1:4, c(2L, 2L)), array(1:6, c(3L, 2L)), .axis = 2L)
    Condition
      Error in `rray_combine()`:
      ! Can't find common dimensions at axis 1. `..1` has dimension 2 and `..2` has dimension 3.
    Code
      rray_combine(x = 1L, y = "x", .axis = 1L)
    Condition
      Error in `rray_combine()`:
      ! Can't combine `x` <integer> and `y` <character>.
    Code
      rray_combine(x = NULL, y = 1L, .axis = 1L)
    Condition
      Error in `rray_combine()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_combine(x = mean, y = 1L, .axis = 1L)
    Condition
      Error in `rray_combine()`:
      ! `x` must be an array, not a function.
    Code
      rray_combine(x = structure(array(1L), class = "foo"), y = 1L, .axis = 1L)
    Condition
      Error in `rray_combine()`:
      ! `x` must be a bare array, not a <foo> object.

