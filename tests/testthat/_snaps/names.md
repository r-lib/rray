# errors on non-vector types

    Code
      rray_names(NULL)
    Condition
      Error in `rray_names()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_names(mean)
    Condition
      Error in `rray_names()`:
      ! `x` must be an array, not a function.

# errors on classed input

    Code
      rray_names(x)
    Condition
      Error in `rray_names()`:
      ! `x` must be a bare array, not a <foo> object.

