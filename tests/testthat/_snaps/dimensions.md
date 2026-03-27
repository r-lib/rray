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

