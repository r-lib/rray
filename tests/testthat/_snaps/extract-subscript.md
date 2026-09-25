# checks positions against the size of `dimensions`

    Code
      rray_as_extract_subscript(7L, c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must not contain values greater than 6.
    Code
      rray_as_extract_subscript(-7L, c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must not contain values less than -6.
    Code
      rray_as_extract_subscript(1L, c(2L, 0L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must not contain values greater than 0.

# checks position signs

    Code
      rray_as_extract_subscript(c(-1L, 2L), 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` can't mix positive and negative values.
    Code
      rray_as_extract_subscript(c(-1L, NA), 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` can't mix negative and missing values.

# checks double positions are whole integers

    Code
      rray_as_extract_subscript(1.5, 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Can't convert from `i` <double> to <integer> due to loss of precision.
    Code
      rray_as_extract_subscript(-1.5, 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Can't convert from `i` <double> to <integer> due to loss of precision.

# checks huge double positions against the size of `dimensions`

    Code
      rray_as_extract_subscript(1e+10, 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must not contain values greater than 3.
    Code
      rray_as_extract_subscript(Inf, 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must not contain values greater than 3.
    Code
      rray_as_extract_subscript(-Inf, 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must not contain values less than -3.

# checks the size of a logical mask

    Code
      rray_as_extract_subscript(c(TRUE, FALSE), 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Logical `i` must be size 1 or 3, not 2.
    Code
      rray_as_extract_subscript(logical(), 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Logical `i` must be size 1 or 3, not 0.
    Code
      rray_as_extract_subscript(array(TRUE, 3L), c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Logical `i` must be size 1 or 6, not 3.

# checks the dimensions of a logical array

    Code
      rray_as_extract_subscript(array(TRUE, c(3L, 2L)), c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Logical `i` must be a vector or have the same dimensions as `x`.
    Code
      rray_as_extract_subscript(array(TRUE, c(2L, 3L, 1L)), c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Logical `i` must be a vector or have the same dimensions as `x`.
    Code
      rray_as_extract_subscript(array(TRUE, c(6L, 1L)), 6L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Logical `i` must be a vector or have the same dimensions as `x`.

# requires one point matrix column per axis

    Code
      rray_as_extract_subscript(matrix(1L, 1L, 3L), c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Numeric matrix `i` must have 2 columns, one for each axis of `x`, not 3.
    Code
      rray_as_extract_subscript(matrix(1L, 1L, 2L), 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Numeric matrix `i` must have 1 column, one for each axis of `x`, not 2.

# checks point coordinates against each axis

    Code
      rray_as_extract_subscript(rbind(c(3L, 1L)), c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Column 1 of `i` must not contain values greater than 2.
    Code
      rray_as_extract_subscript(rbind(c(1L, 4L)), c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Column 2 of `i` must not contain values greater than 3.
    Code
      rray_as_extract_subscript(rbind(c(0L, 1L)), c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Column 1 of `i` must only contain positive values or missing values.
    Code
      rray_as_extract_subscript(rbind(c(1L, -1L)), c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Column 2 of `i` must only contain positive values or missing values.
    Code
      rray_as_extract_subscript(rbind(c(1L, 1L)), c(2L, 0L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Column 2 of `i` must not contain values greater than 0.

# checks double point coordinates are whole integers

    Code
      rray_as_extract_subscript(rbind(c(1, 1.5)), c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Can't convert from `i` <double> to <integer> due to loss of precision.

# errors on numeric arrays with more than two dimensions

    Code
      rray_as_extract_subscript(array(1L, c(1L, 1L, 1L)), c(1L, 1L, 1L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Numeric `i` must be a vector or a matrix, not an array with 3 dimensions.
    Code
      rray_as_extract_subscript(array(1, c(1L, 1L, 1L, 1L)), 1L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! Numeric `i` must be a vector or a matrix, not an array with 4 dimensions.

# errors on unsupported types

    Code
      rray_as_extract_subscript(NULL, 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must be logical, integer, or double, not `NULL`.
    Code
      rray_as_extract_subscript("a", 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must be logical, integer, or double, not the string "a".
    Code
      rray_as_extract_subscript(matrix("a", 1L, 2L), c(2L, 3L))
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must be logical, integer, or double, not a character matrix.
    Code
      rray_as_extract_subscript(0+1i, 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must be logical, integer, or double, not the complex number 0+1i.
    Code
      rray_as_extract_subscript(as.raw(1L), 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must be logical, integer, or double, not the raw value 01.
    Code
      rray_as_extract_subscript(list(1L), 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must be logical, integer, or double, not a list.

# errors on classed input

    Code
      rray_as_extract_subscript(factor("a"), 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must be a bare array, not a <factor> object.
    Code
      rray_as_extract_subscript(structure(1L, class = "foo"), 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must be a bare array, not a <foo> object.
    Code
      rray_as_extract_subscript(data.frame(x = 1L), 3L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `i` must be a bare array, not a <data.frame> object.

# checks `dimensions`

    Code
      rray_as_extract_subscript(1L, -1L)
    Condition
      Error in `rray_as_extract_subscript()`:
      ! `dimensions` must not contain negative values.
    Code
      rray_as_extract_subscript(1L, "a")
    Condition
      Error:
      ! Can't convert `dimensions` <character> to <integer>.

