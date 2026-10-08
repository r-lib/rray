# validates inputs and axis

    Code
      rray_stack(.axis = 1L)
    Condition
      Error in `rray_stack()`:
      ! Must supply at least one array to `...`.
    Code
      rray_stack(x, .axis = integer())
    Condition
      Error in `rray_stack()`:
      ! `.axis` must be a single integer, not length 0.
    Code
      rray_stack(x, .axis = c(1L, 2L))
    Condition
      Error in `rray_stack()`:
      ! `.axis` must be a single integer, not length 2.
    Code
      rray_stack(x, .axis = NA_integer_)
    Condition
      Error in `rray_stack()`:
      ! `.axis` must not be missing.
    Code
      rray_stack(x, .axis = structure(1L, foo = "bar"))
    Condition
      Error in `rray_stack()`:
      ! `.axis` can't have attributes.
    Code
      rray_stack(x, .axis = 1.5)
    Condition
      Error in `rray_stack()`:
      ! `.axis` must contain whole numbers that fit in an integer. Problem at location 1.
    Code
      rray_stack(x, .axis = 0L)
    Condition
      Error in `rray_stack()`:
      ! `.axis` must be greater than or equal to 1, not 0.
    Code
      rray_stack(x, .axis = 4L)
    Condition
      Error in `rray_stack()`:
      ! `.axis` must be less than or equal to the dimensionality of 3, not 4.

# rejects a dimensionality above the maximum

    Code
      rray_stack(x, x, .axis = 1L)
    Condition
      Error in `rray_stack()`:
      ! rray can't support arrays with a dimensionality greater than 64. A dimensionality of 65 was requested.

# rejects incompatible inputs

    Code
      rray_stack(array(1:4, c(2L, 2L)), array(1:6, c(3L, 2L)), .axis = 1L)
    Condition
      Error in `rray_stack()`:
      ! Can't find common dimensions at axis 1. `..1` has dimension 2 and `..2` has dimension 3.
    Code
      rray_stack(x = array(1, c(2L, 2L)), y = array(1, c(3L, 2L)), .axis = 3L)
    Condition
      Error in `rray_stack()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 2 and `y` has dimension 3.
    Code
      rray_stack(x = 1L, y = "x", .axis = 1L)
    Condition
      Error in `rray_stack()`:
      ! Can't combine `x` <integer> and `y` <character>.
    Code
      rray_stack(x = NULL, y = 1L, .axis = 1L)
    Condition
      Error in `rray_stack()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_stack(x = mean, y = 1L, .axis = 1L)
    Condition
      Error in `rray_stack()`:
      ! `x` must be an array, not a function.
    Code
      rray_stack(x = structure(array(1L), class = "foo"), y = 1L, .axis = 1L)
    Condition
      Error in `rray_stack()`:
      ! `x` must be a bare array, not a <foo> object.

