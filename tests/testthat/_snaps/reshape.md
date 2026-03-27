# errors when size would change

    Code
      rray_reshape(1:6, c(6L, 2L))
    Condition
      Error in `rray_reshape()`:
      ! Can't reshape to these dimensions. Can't change from a size of 6 to a size of 12.

# errors on non-array input

    Code
      rray_reshape(NULL, 1L)
    Condition
      Error in `rray_reshape()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_reshape(mean, 1L)
    Condition
      Error in `rray_reshape()`:
      ! `x` must be an array, not a function.

# coerces dimensions to integer

    Code
      rray_reshape(1, 2.5)
    Condition
      Error:
      ! Can't convert from `dimensions` <double> to <integer> due to loss of precision.
      * Locations: 1

# errors on non-coercible dimensions

    Code
      rray_reshape(1, "a")
    Condition
      Error:
      ! Can't convert `dimensions` <character> to <integer>.

# errors on empty dimensions

    Code
      rray_reshape(1, integer())
    Condition
      Error in `rray_reshape()`:
      ! `dimensions` must have at least one element.

# errors on missing dimensions

    Code
      rray_reshape(1, NA_integer_)
    Condition
      Error in `rray_reshape()`:
      ! `dimensions` must not contain missing values.

# errors on negative dimensions

    Code
      rray_reshape(1, -1L)
    Condition
      Error in `rray_reshape()`:
      ! `dimensions` must not contain negative values.

