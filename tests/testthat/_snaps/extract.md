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

