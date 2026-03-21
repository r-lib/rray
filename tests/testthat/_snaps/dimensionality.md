# errors on NULL

    Code
      rray_dimensionality(NULL)
    Condition
      Error in `rray_dimensionality()`:
      ! `x` must be an array, not `NULL`.

# errors on non-vector types

    Code
      rray_dimensionality(mean)
    Condition
      Error in `rray_dimensionality()`:
      ! `x` must be an array, not a function.

---

    Code
      rray_dimensionality(quote(x))
    Condition
      Error in `rray_dimensionality()`:
      ! `x` must be an array, not a symbol.

---

    Code
      rray_dimensionality(environment())
    Condition
      Error in `rray_dimensionality()`:
      ! `x` must be an array, not an environment.

