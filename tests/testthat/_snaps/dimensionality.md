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

# errors on classed input

    Code
      rray_dimensionality(x)
    Condition
      Error in `rray_dimensionality()`:
      ! `x` must be a bare array, not a <foo> object.

