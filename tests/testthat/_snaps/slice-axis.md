# reports subscript errors as `i`

    Code
      rray_slice_axis(x, 4L, axis = 2)
    Condition
      Error in `rray_slice_axis()`:
      ! `i` must not contain values greater than 3.
    Code
      rray_slice_axis(x, "z", axis = 1)
    Condition
      Error in `rray_slice_axis()`:
      ! `i` must only contain names of the axis, not "z".
    Code
      rray_slice_axis(x, "c", axis = 2)
    Condition
      Error in `rray_slice_axis()`:
      ! Character `i` can't select from an axis without names.
    Code
      rray_slice_axis(x, c(TRUE, FALSE, TRUE), axis = 1)
    Condition
      Error in `rray_slice_axis()`:
      ! Logical `i` must be size 1 or 2, not 3.
    Code
      rray_slice_axis(x, matrix(1L), axis = 1)
    Condition
      Error in `rray_slice_axis()`:
      ! `i` must be a vector or a 1D array, not an array with a dimensionality of 2.
    Code
      rray_slice_axis(x, list(1L), axis = 1)
    Condition
      Error in `rray_slice_axis()`:
      ! `i` must be logical, integer, double, character, or `NULL`, not a list.

# `axis` is validated

    Code
      rray_slice_axis(x, 1L, axis = 0)
    Condition
      Error in `rray_slice_axis()`:
      ! `axis` must be greater than or equal to 1, not 0.
    Code
      rray_slice_axis(x, 1L, axis = 3)
    Condition
      Error in `rray_slice_axis()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.
    Code
      rray_slice_axis(x, 1L, axis = c(1, 2))
    Condition
      Error in `rray_slice_axis()`:
      ! `axis` must be a single integer, not length 2.
    Code
      rray_slice_axis(x, 1L, axis = NA_integer_)
    Condition
      Error in `rray_slice_axis()`:
      ! `axis` must not be missing.

# errors on unsupported `x` inputs

    Code
      rray_slice_axis(NULL, 1L, axis = 1)
    Condition
      Error in `rray_slice_axis()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_slice_axis(structure(1:2, class = "foo"), 1L, axis = 1)
    Condition
      Error in `rray_slice_axis()`:
      ! `x` must be a bare array, not a <foo> object.

# reports errors from `rray_slice_rows()`

    Code
      rray_slice_rows(array(1:6, c(2L, 3L)), 3L)
    Condition
      Error in `rray_slice_rows()`:
      ! `i` must not contain values greater than 2.

# errors if `x` doesn't have a second axis

    Code
      rray_slice_columns(1:3, 1L)
    Condition
      Error in `rray_slice_columns()`:
      ! `axis` must be less than or equal to the dimensionality of 1, not 2.

# reports subscript errors from `rray_slice_assign_axis()` as `i`

    Code
      rray_slice_assign_axis(x, 4L, axis = 2, value = 0L)
    Condition
      Error in `rray_slice_assign_axis()`:
      ! `i` must not contain values greater than 3.
    Code
      rray_slice_assign_axis(x, "z", axis = 1, value = 0L)
    Condition
      Error in `rray_slice_assign_axis()`:
      ! `i` must only contain names of the axis, not "z".

# `axis` and `value` are validated by `rray_slice_assign_axis()`

    Code
      rray_slice_assign_axis(x, 1L, axis = 3, value = 0L)
    Condition
      Error in `rray_slice_assign_axis()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.
    Code
      rray_slice_assign_axis(x, 1L, axis = 1, value = "a")
    Condition
      Error in `rray_slice_assign_axis()`:
      ! Can't convert from `value` <character> to <integer>.
    Code
      rray_slice_assign_axis(x, 1L, axis = 1, value = 1:2)
    Condition
      Error in `rray_slice_assign_axis()`:
      ! Can't broadcast axis 1 of `value` from dimension 2 to 1.

# errors on unsupported `x` inputs to `rray_slice_assign_axis()`

    Code
      rray_slice_assign_axis(NULL, 1L, axis = 1, value = 0L)
    Condition
      Error in `rray_slice_assign_axis()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_slice_assign_axis(structure(1:2, class = "foo"), 1L, axis = 1, value = 0L)
    Condition
      Error in `rray_slice_assign_axis()`:
      ! `x` must be a bare array, not a <foo> object.

# reports errors from `rray_slice_assign_rows()`

    Code
      rray_slice_assign_rows(array(1:6, c(2L, 3L)), 3L, 0L)
    Condition
      Error in `rray_slice_assign_rows()`:
      ! `i` must not contain values greater than 2.

# `rray_slice_assign_columns()` errors without a second axis

    Code
      rray_slice_assign_columns(1:3, 1L, 0L)
    Condition
      Error in `rray_slice_assign_columns()`:
      ! `axis` must be less than or equal to the dimensionality of 1, not 2.

