# errors on invalid `permutation`

    Code
      rray_transpose(x, 1L)
    Condition
      Error in `rray_transpose()`:
      ! `permutation` must have length 2 to match the dimensionality of the array, not length 1.
    Code
      rray_transpose(x, c(1L, 2L, 3L))
    Condition
      Error in `rray_transpose()`:
      ! `permutation` must have length 2 to match the dimensionality of the array, not length 3.
    Code
      rray_transpose(x, c(1L, 1L))
    Condition
      Error in `rray_transpose()`:
      ! `permutation` must not contain the axis 1 more than once.
    Code
      rray_transpose(x, c(1L, 3L))
    Condition
      Error in `rray_transpose()`:
      ! `permutation` must contain values less than or equal to the dimensionality of 2, not 3.
    Code
      rray_transpose(x, c(1L, 0L))
    Condition
      Error in `rray_transpose()`:
      ! `permutation` must contain values greater than or equal to 1, not 0.
    Code
      rray_transpose(x, c(1L, NA_integer_))
    Condition
      Error in `rray_transpose()`:
      ! `permutation` must not contain missing values.
    Code
      rray_transpose(x, structure(c(1L, 2L), foo = "bar"))
    Condition
      Error in `rray_transpose()`:
      ! `permutation` can't have attributes.
    Code
      rray_transpose(x, c(1.5, 2))
    Condition
      Error:
      ! Can't convert from `permutation` <double> to <integer> due to loss of precision.
      * Locations: 1
    Code
      rray_transpose(x, "x")
    Condition
      Error:
      ! Can't convert `permutation` <character> to <integer>.

# errors on non-array input

    Code
      rray_transpose(NULL)
    Condition
      Error in `rray_transpose()`:
      ! `x` must be an array, not `NULL`.
    Code
      rray_transpose(mean)
    Condition
      Error in `rray_transpose()`:
      ! `x` must be an array, not a function.

# errors on classed input

    Code
      rray_transpose(x)
    Condition
      Error in `rray_transpose()`:
      ! `x` must be a bare array, not a <foo> object.

