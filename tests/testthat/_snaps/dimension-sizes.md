# errors on NULL

    Code
      rray_dimension_sizes(NULL)
    Condition
      Error in `rray_dimension_sizes()`:
      ! `x` must be an array, not `NULL`.

# errors on non-vector types

    Code
      rray_dimension_sizes(mean)
    Condition
      Error in `rray_dimension_sizes()`:
      ! `x` must be an array, not a function.

---

    Code
      rray_dimension_sizes(quote(x))
    Condition
      Error in `rray_dimension_sizes()`:
      ! `x` must be an array, not a symbol.

---

    Code
      rray_dimension_sizes(environment())
    Condition
      Error in `rray_dimension_sizes()`:
      ! `x` must be an array, not an environment.

