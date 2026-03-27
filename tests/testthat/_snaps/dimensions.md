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

