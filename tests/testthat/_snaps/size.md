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
      ! Can't convert long vector `x` (size 2147483648) to a 1D array.

# errors on classed input

    Code
      rray_size(x)
    Condition
      Error in `rray_size()`:
      ! `x` must be a bare array, not a <foo> object.

