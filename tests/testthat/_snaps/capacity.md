# errors on non-vector types

    Code
      rray_capacity(NULL)
    Condition
      Error:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_capacity(mean)
    Condition
      Error:
      ! `x` must be an array, not a function.

