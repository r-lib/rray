# reports subscript errors from `rray_extract()`

    Code
      rray_extract(x, 7L)
    Condition
      Error in `rray_extract()`:
      ! `i` must not contain values greater than 6.
    Code
      rray_extract(x, rbind(c(1L, 4L)))
    Condition
      Error in `rray_extract()`:
      ! Column 2 of `i` must not contain values greater than 3.

# errors on unsupported `x` inputs

    Code
      rray_extract(NULL, 1L)
    Condition
      Error in `rray_extract()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_extract(mean, 1L)
    Condition
      Error in `rray_extract()`:
      ! `x` must be an array, not a function.
    Code
      rray_extract(structure(1:2, class = "foo"), 1L)
    Condition
      Error in `rray_extract()`:
      ! `x` must be a bare array, not a <foo> object.

# casts `value` to the type of `x`

    Code
      rray_extract_assign(array(1:3), 2L, 1.5)
    Condition
      Error in `rray_extract_assign()`:
      ! Can't convert from `value` <double> to <integer> due to loss of precision at location 1.
    Code
      rray_extract_assign(array(1:3), 2L, "a")
    Condition
      Error in `rray_extract_assign()`:
      ! Can't convert from `value` <character> to <integer>.
    Code
      rray_extract_assign(array(1:3), 2L, NULL)
    Condition
      Error in `rray_extract_assign()`:
      ! `value` must be an array, not `NULL`.
    Code
      rray_extract_assign(array(1:3), 2L, factor("a"))
    Condition
      Error in `rray_extract_assign()`:
      ! `value` must be a bare array, not a <factor> object.

# broadcasts `value` to the number of selected values

    Code
      rray_extract_assign(x, 1:4, 1:2)
    Condition
      Error in `rray_extract_assign()`:
      ! Can't broadcast axis 1 of `value` from dimension 2 to 4.
    Code
      rray_extract_assign(x, 1L, integer())
    Condition
      Error in `rray_extract_assign()`:
      ! Can't broadcast axis 1 of `value` from dimension 0 to 1.
    Code
      rray_extract_assign(x, 1L, matrix(0L))
    Condition
      Error in `rray_extract_assign()`:
      ! Can't broadcast `value` from dimensionality 2 to 1. Can't decrease dimensionality.
    Code
      rray_extract_assign(x, c(1L, NA_integer_, 6L), c(10L, 60L))
    Condition
      Error in `rray_extract_assign()`:
      ! Can't broadcast axis 1 of `value` from dimension 2 to 3.
    Code
      rray_extract_assign(x, rep(NA, 6L), 1:2)
    Condition
      Error in `rray_extract_assign()`:
      ! Can't broadcast axis 1 of `value` from dimension 2 to 6.

# reports subscript errors from `rray_extract_assign()`

    Code
      rray_extract_assign(x, 7L, 0L)
    Condition
      Error in `rray_extract_assign()`:
      ! `i` must not contain values greater than 6.
    Code
      rray_extract_assign(x, rbind(c(0L, 1L)), 0L)
    Condition
      Error in `rray_extract_assign()`:
      ! Column 1 of `i` must only contain positive values or missing values.
    Code
      rray_extract_assign(x, rbind(c(NA_integer_, 4L)), 0L)
    Condition
      Error in `rray_extract_assign()`:
      ! Column 2 of `i` must not contain values greater than 3.
    Code
      rray_extract_assign(x, "a", 0L)
    Condition
      Error in `rray_extract_assign()`:
      ! `i` must be logical, integer, or double, not the string "a".

# errors on unsupported `x` inputs to `rray_extract_assign()`

    Code
      rray_extract_assign(NULL, 1L, 0L)
    Condition
      Error in `rray_extract_assign()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_extract_assign(structure(1:2, class = "foo"), 1L, 0L)
    Condition
      Error in `rray_extract_assign()`:
      ! `x` must be a bare array, not a <foo> object.

