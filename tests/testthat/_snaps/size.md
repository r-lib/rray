# errors on non-vector types

    Code
      rray_size(NULL)
    Condition
      Error in `rray_size()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_size(mean)
    Condition
      Error in `rray_size()`:
      ! `x` must be an array, not a function.

# errors on long vectors without `dim`

    Code
      rray_size(raw(2^31))
    Condition
      Error in `rray_size()`:
      ! `x` can't have more than 2147483647 elements without a `dim`.

# errors on classed input

    Code
      rray_size(x)
    Condition
      Error in `rray_size()`:
      ! `x` must be a bare array, not a <foo> object.

