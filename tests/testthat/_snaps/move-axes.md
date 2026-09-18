# `from` and `to` must be named

    Code
      rray_move_axes(x, 1L, 3L)
    Condition
      Error in `rray_move_axes()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 1L
      * ..2 = 3L
      i Did you forget to name an argument?
    Code
      rray_move_axes(x, 1L, to = 3L)
    Condition
      Error in `rray_move_axes()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1L
      i Did you forget to name an argument?

# errors on invalid `from`

    Code
      rray_move_axes(x, from = c(1L, 1L), to = c(1L, 2L))
    Condition
      Error in `rray_move_axes()`:
      ! `from` must not contain 1 more than once.
    Code
      rray_move_axes(x, from = 4L, to = 1L)
    Condition
      Error in `rray_move_axes()`:
      ! `from` must contain values less than or equal to the dimensionality of 3, not 4.
    Code
      rray_move_axes(x, from = 0L, to = 1L)
    Condition
      Error in `rray_move_axes()`:
      ! `from` must contain values greater than or equal to 1, not 0.
    Code
      rray_move_axes(x, from = NA_integer_, to = 1L)
    Condition
      Error in `rray_move_axes()`:
      ! `from` must not contain missing values.
    Code
      rray_move_axes(x, from = structure(1L, foo = "bar"), to = 1L)
    Condition
      Error in `rray_move_axes()`:
      ! `from` can't have attributes.
    Code
      rray_move_axes(x, from = 1.5, to = 1L)
    Condition
      Error:
      ! Can't convert from `from` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      rray_move_axes(x, from = "x", to = 1L)
    Condition
      Error:
      ! Can't convert `from` <character> to <integer>.

# errors on invalid `to`

    Code
      rray_move_axes(x, from = c(1L, 2L), to = c(3L, 3L))
    Condition
      Error in `rray_move_axes()`:
      ! `to` must not contain 3 more than once.
    Code
      rray_move_axes(x, from = 1L, to = 4L)
    Condition
      Error in `rray_move_axes()`:
      ! `to` must contain values less than or equal to the dimensionality of 3, not 4.
    Code
      rray_move_axes(x, from = 1L, to = 0L)
    Condition
      Error in `rray_move_axes()`:
      ! `to` must contain values greater than or equal to 1, not 0.
    Code
      rray_move_axes(x, from = 1L, to = NA_integer_)
    Condition
      Error in `rray_move_axes()`:
      ! `to` must not contain missing values.
    Code
      rray_move_axes(x, from = 1L, to = structure(1L, foo = "bar"))
    Condition
      Error in `rray_move_axes()`:
      ! `to` can't have attributes.
    Code
      rray_move_axes(x, from = 1L, to = 1.5)
    Condition
      Error:
      ! Can't convert from `to` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      rray_move_axes(x, from = 1L, to = "x")
    Condition
      Error:
      ! Can't convert `to` <character> to <integer>.

# `from` and `to` must be the same length

    Code
      rray_move_axes(x, from = c(1L, 2L), to = 1L)
    Condition
      Error in `rray_move_axes()`:
      ! `from` (2) and `to` (1) must be the same length.
    Code
      rray_move_axes(x, from = 1L, to = c(1L, 2L))
    Condition
      Error in `rray_move_axes()`:
      ! `from` (1) and `to` (2) must be the same length.
    Code
      rray_move_axes(x, from = 1L, to = integer())
    Condition
      Error in `rray_move_axes()`:
      ! `from` (1) and `to` (0) must be the same length.

# errors on non-array input

    Code
      rray_move_axes(NULL, from = 1L, to = 1L)
    Condition
      Error in `rray_move_axes()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_move_axes(mean, from = 1L, to = 1L)
    Condition
      Error in `rray_move_axes()`:
      ! `x` must be an array, not a function.

# errors on classed input

    Code
      rray_move_axes(x, from = 1L, to = 1L)
    Condition
      Error in `rray_move_axes()`:
      ! `x` must be a bare array, not a <foo> object.

