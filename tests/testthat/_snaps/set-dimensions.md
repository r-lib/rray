# errors when size would change

    Code
      rray_set_dimensions(1:6, c(6L, 2L))
    Condition
      Error in `rray_set_dimensions()`:
      ! Can't set these dimensions. Can't change from a size of 6 to a size of 12.

# errors on non-array input

    Code
      rray_set_dimensions(NULL, 1L)
    Condition
      Error in `rray_set_dimensions()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_set_dimensions(mean, 1L)
    Condition
      Error in `rray_set_dimensions()`:
      ! `x` must be an array, not a function.

# coerces dimensions to integer

    Code
      rray_set_dimensions(1, 2.5)
    Condition
      Error:
      ! Can't convert from `dimensions` <double> to <integer> due to loss of precision.
      * Locations: 1

# errors on non-coercible dimensions

    Code
      rray_set_dimensions(1, "a")
    Condition
      Error:
      ! Can't convert `dimensions` <character> to <integer>.

# errors on empty dimensions

    Code
      rray_set_dimensions(1, integer())
    Condition
      Error in `rray_set_dimensions()`:
      ! `dimensions` must have at least one element.

# errors on missing dimensions

    Code
      rray_set_dimensions(1, NA_integer_)
    Condition
      Error in `rray_set_dimensions()`:
      ! `dimensions` must not contain missing values.

# errors on negative dimensions

    Code
      rray_set_dimensions(1, -1L)
    Condition
      Error in `rray_set_dimensions()`:
      ! `dimensions` must not contain negative values.

