# checks flat positions

    Code
      rray_extract(x, 7L)
    Condition
      Error in `rray_extract()`:
      ! Can't subset elements past the end.
      i Location 7 doesn't exist.
      i There are only 6 elements.
    Code
      rray_extract(x, c(-1L, 2L))
    Condition
      Error in `rray_extract()`:
      ! Can't subset elements with `i`.
      x Negative and positive locations can't be mixed.
      i Subscript `i` has a positive value at location 2.
    Code
      rray_extract(x, c(-1L, NA))
    Condition
      Error in `rray_extract()`:
      ! Can't subset elements with `i`.
      x Negative locations can't have missing values.
      i Subscript `i` has a missing value at location 2.
    Code
      rray_extract(x, 1.5)
    Condition
      Error in `rray_extract()`:
      ! Can't subset elements with `i`.
      x Can't convert from `i` <double> to <integer> due to loss of precision.

# requires a logical mask of size 1 or the size of `x`

    Code
      rray_extract(x, c(TRUE, FALSE))
    Condition
      Error in `rray_extract()`:
      ! Can't subset elements with `i`.
      x Logical subscript `i` must be size 1 or 6, not 2.
    Code
      rray_extract(x, array(TRUE, 3L))
    Condition
      Error in `rray_extract()`:
      ! Can't subset elements with `i`.
      x Logical subscript `i` must be size 1 or 6, not 3.

# requires a logical array to match the dimensions of `x`

    Code
      rray_extract(x, array(TRUE, c(3L, 2L)))
    Condition
      Error in `rray_extract()`:
      ! Logical `i` must be a vector or have the same dimensions as `x`.
    Code
      rray_extract(x, array(TRUE, c(2L, 3L, 1L)))
    Condition
      Error in `rray_extract()`:
      ! Logical `i` must be a vector or have the same dimensions as `x`.

# requires one point matrix column per axis of `x`

    Code
      rray_extract(array(1:6, c(2L, 3L)), matrix(1L, 1L, 3L))
    Condition
      Error in `rray_extract()`:
      ! Numeric matrix `i` must have 2 columns, one for each axis of `x`, not 3.
    Code
      rray_extract(1:3, matrix(1L, 1L, 2L))
    Condition
      Error in `rray_extract()`:
      ! Numeric matrix `i` must have 1 column, one for each axis of `x`, not 2.

# checks point coordinates

    Code
      rray_extract(x, rbind(c(1L, 4L)))
    Condition
      Error in `rray_extract()`:
      ! `i[, 2]` must not contain values greater than 3.
    Code
      rray_extract(x, rbind(c(0L, 1L)))
    Condition
      Error in `rray_extract()`:
      ! `i[, 1]` must only contain positive values or missing values.
    Code
      rray_extract(x, rbind(c(1L, -1L)))
    Condition
      Error in `rray_extract()`:
      ! `i[, 2]` must only contain positive values or missing values.
    Code
      rray_extract(x, rbind(c(1, 1.5)))
    Condition
      Error:
      ! Can't convert from `i[, 2]` <double> to <integer> due to loss of precision.
      * Locations: 1

# errors on numeric arrays with more than two dimensions

    Code
      rray_extract(x, array(1L, c(1L, 1L, 1L)))
    Condition
      Error in `rray_extract()`:
      ! Numeric `i` must be a vector or a matrix, not an array with 3 dimensions.

# errors on unsupported `i` inputs

    Code
      rray_extract(x, "a")
    Condition
      Error in `rray_extract()`:
      ! `i` must be logical, integer, or double, not the string "a".
    Code
      rray_extract(x, matrix("a", 1L, 2L))
    Condition
      Error in `rray_extract()`:
      ! `i` must be logical, integer, or double, not a character matrix.
    Code
      rray_extract(x, 0+1i)
    Condition
      Error in `rray_extract()`:
      ! `i` must be logical, integer, or double, not the complex number 0+1i.
    Code
      rray_extract(x, list(1L))
    Condition
      Error in `rray_extract()`:
      ! `i` must be logical, integer, or double, not a list.
    Code
      rray_extract(x, factor("a"))
    Condition
      Error in `rray_extract()`:
      ! `i` must be a bare array, not a <factor> object.

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

