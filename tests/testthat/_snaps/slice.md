# errors on empty arguments

    Code
      rray_slice(x, , 1L)
    Condition
      Error in `rray_slice()`:
      ! `...` must not contain empty arguments. Use `TRUE` to select a whole axis.
    Code
      rray_slice(x, 1L, )
    Condition
      Error in `rray_slice()`:
      ! `...` must not contain empty arguments. Use `TRUE` to select a whole axis.

# requires one subscript per axis

    Code
      rray_slice(x)
    Condition
      Error in `rray_slice()`:
      ! Must supply exactly 3 subscripts to `...`, not 0.
    Code
      rray_slice(x, 1L, 1L)
    Condition
      Error in `rray_slice()`:
      ! Must supply exactly 3 subscripts to `...`, not 2.
    Code
      rray_slice(x, 1L, 1L, 1L, 1L)
    Condition
      Error in `rray_slice()`:
      ! Must supply exactly 3 subscripts to `...`, not 4.
    Code
      rray_slice(1:3)
    Condition
      Error in `rray_slice()`:
      ! Must supply exactly 1 subscript to `...`, not 0.

# requires unnamed subscripts

    Code
      rray_slice(x, i = 1L, 1L)
    Condition
      Error in `rray_slice()`:
      ! All elements of `...` must be unnamed.

# reports subscript errors with their position

    Code
      rray_slice(x, 1L, 4L)
    Condition
      Error in `rray_slice()`:
      ! `..2` must not contain values greater than 3.
    Code
      rray_slice(x, "z", 1L)
    Condition
      Error in `rray_slice()`:
      ! `..1` must only contain names of the axis, not "z".
    Code
      rray_slice(x, 1L, "c")
    Condition
      Error in `rray_slice()`:
      ! Character `..2` can't select from an axis without names.
    Code
      rray_slice(x, c(TRUE, FALSE, TRUE), 1L)
    Condition
      Error in `rray_slice()`:
      ! Logical `..1` must be size 1 or 2, not 3.
    Code
      rray_slice(x, matrix(1L), 1L)
    Condition
      Error in `rray_slice()`:
      ! `..1` must be a vector or a 1D array, not an array with a dimensionality of 2.

# errors on unsupported `x` inputs

    Code
      rray_slice(NULL, 1L)
    Condition
      Error in `rray_slice()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_slice(mean, 1L)
    Condition
      Error in `rray_slice()`:
      ! `x` must be an array, not a function.
    Code
      rray_slice(structure(1:2, class = "foo"), 1L)
    Condition
      Error in `rray_slice()`:
      ! `x` must be a bare array, not a <foo> object.

