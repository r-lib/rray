# checks locations against `dimension`

    Code
      rray_as_slice_subscript(4L, 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must not contain values greater than 3.
    Code
      rray_as_slice_subscript(-4L, 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must not contain values less than -3.
    Code
      rray_as_slice_subscript(1L, 0L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must not contain values greater than 0.

# checks location signs

    Code
      rray_as_slice_subscript(c(-1L, 2L), 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` can't mix positive and negative values.
    Code
      rray_as_slice_subscript(c(-1L, NA), 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` can't mix negative and missing values.

# checks double locations are whole integers

    Code
      rray_as_slice_subscript(1.5, 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! Can't convert from `i` <double> to <integer> due to loss of precision.

# checks the size of a logical mask

    Code
      rray_as_slice_subscript(c(TRUE, FALSE), 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! Logical `i` must be size 1 or 3, not 2.
    Code
      rray_as_slice_subscript(logical(), 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! Logical `i` must be size 1 or 3, not 0.

# checks character names

    Code
      rray_as_slice_subscript("a", 2L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! Character `i` can't select from an axis without names.
    Code
      rray_as_slice_subscript(c("a", ""), 2L, c("a", "b"))
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must not contain the empty string.
    Code
      rray_as_slice_subscript(c("a", "z"), 2L, c("a", "b"))
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must only contain names of the axis, not "z".

# errors on subscripts with two or more dimensions

    Code
      rray_as_slice_subscript(matrix(1L), 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must be a vector or a 1D array, not an array with a dimensionality of 2.
    Code
      rray_as_slice_subscript(array(TRUE, c(1L, 1L, 1L)), 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must be a vector or a 1D array, not an array with a dimensionality of 3.
    Code
      rray_as_slice_subscript(matrix("a"), 3L, c("a", "b", "c"))
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must be a vector or a 1D array, not an array with a dimensionality of 2.

# errors on unsupported types

    Code
      rray_as_slice_subscript(0+1i, 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must be logical, integer, double, character, or `NULL`, not the complex number 0+1i.
    Code
      rray_as_slice_subscript(as.raw(1L), 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must be logical, integer, double, character, or `NULL`, not the raw value 01.
    Code
      rray_as_slice_subscript(list(1L), 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must be logical, integer, double, character, or `NULL`, not a list.

# errors on classed input

    Code
      rray_as_slice_subscript(factor("a"), 3L, "a")
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must be a bare array, not a <factor> object.
    Code
      rray_as_slice_subscript(structure(1L, class = "foo"), 3L)
    Condition
      Error in `rray_as_slice_subscript()`:
      ! `i` must be a bare array, not a <foo> object.

