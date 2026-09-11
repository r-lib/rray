# errors on invalid `axes`

    Code
      rray_permute_axes(x, NULL)
    Condition
      Error in `rray_permute_axes()`:
      ! `axes` must have length 2 to match the dimensionality of the array, not length 0.
    Code
      rray_permute_axes(x, 1L)
    Condition
      Error in `rray_permute_axes()`:
      ! `axes` must have length 2 to match the dimensionality of the array, not length 1.
    Code
      rray_permute_axes(x, c(1L, 2L, 3L))
    Condition
      Error in `rray_permute_axes()`:
      ! `axes` must have length 2 to match the dimensionality of the array, not length 3.
    Code
      rray_permute_axes(x, c(1L, 1L))
    Condition
      Error in `rray_permute_axes()`:
      ! `axes` must not contain axis 1 more than once.
    Code
      rray_permute_axes(x, c(1L, 3L))
    Condition
      Error in `rray_permute_axes()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.
    Code
      rray_permute_axes(x, c(1L, 0L))
    Condition
      Error in `rray_permute_axes()`:
      ! `axes` must contain values greater than or equal to 1, not 0.
    Code
      rray_permute_axes(x, c(1L, NA_integer_))
    Condition
      Error in `rray_permute_axes()`:
      ! `axes` must not contain missing values.
    Code
      rray_permute_axes(x, structure(c(1L, 2L), foo = "bar"))
    Condition
      Error in `rray_permute_axes()`:
      ! `axes` can't have attributes.
    Code
      rray_permute_axes(x, c(1.5, 2))
    Condition
      Error:
      ! Can't convert from `axes` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      rray_permute_axes(x, "x")
    Condition
      Error:
      ! Can't convert `axes` <character> to <integer>.

# errors on non-array input

    Code
      rray_permute_axes(NULL, 1L)
    Condition
      Error in `rray_permute_axes()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_permute_axes(mean, 1L)
    Condition
      Error in `rray_permute_axes()`:
      ! `x` must be an array, not a function.

# errors on classed input

    Code
      rray_permute_axes(x, 1L)
    Condition
      Error in `rray_permute_axes()`:
      ! `x` must be a bare array, not a <foo> object.

