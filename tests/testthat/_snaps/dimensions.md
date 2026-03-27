# errors on NULL

    Code
      rray_dimensions(NULL)
    Condition
      Error in `rray_dimensions()`:
      ! `x` must be an array, not `NULL`.

# errors on non-vector types

    Code
      rray_dimensions(mean)
    Condition
      Error in `rray_dimensions()`:
      ! `x` must be an array, not a function.

---

    Code
      rray_dimensions(quote(x))
    Condition
      Error in `rray_dimensions()`:
      ! `x` must be an array, not a symbol.

---

    Code
      rray_dimensions(environment())
    Condition
      Error in `rray_dimensions()`:
      ! `x` must be an array, not an environment.

# errors on incompatible dimensions

    Code
      rray_dimensions_common(array(1, c(2, 3)), array(1, c(4, 3)))
    Condition
      Error in `rray_dimensions_common()`:
      ! Can't find common dimensions at axis 1. Dimensions 2 and 4 are incompatible.

# errors on zero non-NULL inputs

    Code
      rray_dimensions_common()
    Condition
      Error in `rray_dimensions_common()`:
      ! Must supply at least one array to `...`.

---

    Code
      rray_dimensions_common(NULL, NULL)
    Condition
      Error in `rray_dimensions_common()`:
      ! Must supply at least one array to `...`.

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

