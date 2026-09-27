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

# errors when `value` can't broadcast to the selection

    Code
      rray_slice_assign(x, TRUE, 1:2, 1L, value = 1:3)
    Condition
      Error in `rray_slice_assign()`:
      ! Can't broadcast axis 1 of `value` from dimension 3 to 2.
    Code
      rray_slice_assign(x, 1L, 1:2, 1L, value = 1:2)
    Condition
      Error in `rray_slice_assign()`:
      ! Can't broadcast axis 1 of `value` from dimension 2 to 1.
    Code
      rray_slice_assign(x, TRUE, TRUE, TRUE, value = 1:4)
    Condition
      Error in `rray_slice_assign()`:
      ! Can't broadcast axis 1 of `value` from dimension 4 to 2.
    Code
      rray_slice_assign(x, c(1L, NA), 1L, 1L, value = 1:3)
    Condition
      Error in `rray_slice_assign()`:
      ! Can't broadcast axis 1 of `value` from dimension 3 to 2.
    Code
      rray_slice_assign(x, 1L, 1L, 1L, value = array(0L, c(1L, 1L, 1L, 1L)))
    Condition
      Error in `rray_slice_assign()`:
      ! Can't broadcast `value` from dimensionality 4 to 3. Can't decrease dimensionality.

# casts `value` to the type of `x`

    Code
      rray_slice_assign(array(1:3), 2L, value = 1.5)
    Condition
      Error in `rray_slice_assign()`:
      ! Can't convert from `value` <double> to <integer> due to loss of precision at location 1.
    Code
      rray_slice_assign(array(1:3), 2L, value = "a")
    Condition
      Error in `rray_slice_assign()`:
      ! Can't convert from `value` <character> to <integer>.
    Code
      rray_slice_assign(array(1:3), 2L, value = NULL)
    Condition
      Error in `rray_slice_assign()`:
      ! `value` must be an array, not `NULL`.

# assignment requires one unnamed subscript per axis

    Code
      rray_slice_assign(x, 1L, value = 0L)
    Condition
      Error in `rray_slice_assign()`:
      ! Must supply exactly 2 subscripts to `...`, not 1.
    Code
      rray_slice_assign(x, 1L, 1L, 1L, value = 0L)
    Condition
      Error in `rray_slice_assign()`:
      ! Must supply exactly 2 subscripts to `...`, not 3.
    Code
      rray_slice_assign(x, i = 1L, 1L, value = 0L)
    Condition
      Error in `rray_slice_assign()`:
      ! All elements of `...` must be unnamed.

# reports subscript errors from `rray_slice_assign()`

    Code
      rray_slice_assign(x, 1L, 4L, value = 0L)
    Condition
      Error in `rray_slice_assign()`:
      ! `..2` must not contain values greater than 3.
    Code
      rray_slice_assign(x, "z", 1L, value = 0L)
    Condition
      Error in `rray_slice_assign()`:
      ! `..1` must only contain names of the axis, not "z".
    Code
      rray_slice_assign(x, c(-1L, NA), 1L, value = 0L)
    Condition
      Error in `rray_slice_assign()`:
      ! `..1` can't mix negative and missing values.

# errors on unsupported `x` inputs to `rray_slice_assign()`

    Code
      rray_slice_assign(NULL, 1L, value = 0L)
    Condition
      Error in `rray_slice_assign()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_slice_assign(structure(1:2, class = "foo"), 1L, value = 0L)
    Condition
      Error in `rray_slice_assign()`:
      ! `x` must be a bare array, not a <foo> object.

