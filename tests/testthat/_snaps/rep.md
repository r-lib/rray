# `times` and `axes` must be named

    Code
      rray_rep(x, 2, 1)
    Condition
      Error in `rray_rep()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 2
      * ..2 = 1
      i Did you forget to name an argument?
    Code
      rray_rep(x, 2, axes = 1)
    Condition
      Error in `rray_rep()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 2
      i Did you forget to name an argument?
    Code
      rray_rep(x, times = 2, axis = 1)
    Condition
      Error in `rray_rep()`:
      ! `...` must be empty.
      x Problematic argument:
      * axis = 1

# `axes` is validated

    Code
      rray_rep(x, times = 2, axes = c(1, 1))
    Condition
      Error in `rray_rep()`:
      ! `axes` must be in strictly increasing order.

---

    Code
      rray_rep(x, times = 2, axes = c(2, 1))
    Condition
      Error in `rray_rep()`:
      ! `axes` must be in strictly increasing order.

---

    Code
      rray_rep(x, times = 2, axes = 0)
    Condition
      Error in `rray_rep()`:
      ! `axes` must contain values greater than or equal to 1, not 0.

---

    Code
      rray_rep(x, times = 2, axes = 3)
    Condition
      Error in `rray_rep()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_rep(x, times = 2, axes = NA)
    Condition
      Error in `rray_rep()`:
      ! `axes` must not contain missing values.

# `times` is validated

    Code
      rray_rep(x, times = NA_integer_, axes = 1)
    Condition
      Error in `rray_rep()`:
      ! `times` must not contain missing values.

---

    Code
      rray_rep(x, times = -1, axes = 1)
    Condition
      Error in `rray_rep()`:
      ! `times` must not contain negative values.

---

    Code
      rray_rep(x, times = 1.5, axes = 1)
    Condition
      Error:
      ! Can't convert from `times` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      rray_rep(x, times = structure(1L, names = "a"), axes = 1)
    Condition
      Error in `rray_rep()`:
      ! `times` can't have attributes.

---

    Code
      rray_rep(x, times = c(1, 2), axes = 1)
    Condition
      Error in `rray_rep()`:
      ! `times` must be size 1, not size 2.

---

    Code
      rray_rep(x, times = c(1, 2, 3), axes = c(1, 2))
    Condition
      Error in `rray_rep()`:
      ! `times` must be size 1 or size 2 to match `axes`, not size 3.

---

    Code
      rray_rep(x, times = integer(), axes = 1)
    Condition
      Error in `rray_rep()`:
      ! `times` must be size 1, not size 0.

---

    Code
      rray_rep(x, times = integer(), axes = c(1, 2))
    Condition
      Error in `rray_rep()`:
      ! `times` must be size 1 or size 2 to match `axes`, not size 0.

---

    Code
      rray_rep_each(x, times = NA_integer_, axis = 1)
    Condition
      Error in `rray_rep_each()`:
      ! `times` must not contain missing values.

---

    Code
      rray_rep_each(x, times = c(1, -1, 1), axis = 1)
    Condition
      Error in `rray_rep_each()`:
      ! `times` must not contain negative values.

---

    Code
      rray_rep_each(x, times = c(1, 2), axis = 1)
    Condition
      Error in `rray_rep_each()`:
      ! `times` must be size 1 or the `axis` dimension of 3, not size 2.

---

    Code
      rray_rep_each(x, times = integer(), axis = 1)
    Condition
      Error in `rray_rep_each()`:
      ! `times` must be size 1 or the `axis` dimension of 3, not size 0.

# errors if the repeated dimension is too large

    Code
      rray_rep(x, times = .Machine$integer.max, axes = 1)
    Condition
      Error in `rray_rep()`:
      ! The dimension implied by `times` is too large for R.

---

    Code
      rray_rep_each(x, times = max, axis = 1)
    Condition
      Error in `rray_rep_each()`:
      ! The dimension implied by `times` is too large for R.

---

    Code
      rray_rep_each(x, times = c(max, max), axis = 1)
    Condition
      Error in `rray_rep_each()`:
      ! The dimension implied by `times` is too large for R.

# errors if the repeated size is too large

    Code
      rray_rep(x, times = 2^30, axes = c(1, 2))
    Condition
      Error in `rray_rep()`:
      ! Size (1.15292e+18) computed from dimensions `(1073741824, 1073741824)` is too large.

# errors on invalid input

    Code
      rray_rep(NULL, times = 2, axes = 1)
    Condition
      Error in `rray_rep()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_rep(x, times = 2, axes = 1)
    Condition
      Error in `rray_rep()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_rep_each(NULL, times = 2, axis = 1)
    Condition
      Error in `rray_rep_each()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_rep_each(x, times = 2, axis = 1)
    Condition
      Error in `rray_rep_each()`:
      ! `x` must be a bare array, not a <foo> object.

# `times` and `axis` must be named

    Code
      rray_rep_each(x, 2, 1)
    Condition
      Error in `rray_rep_each()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 2
      * ..2 = 1
      i Did you forget to name an argument?
    Code
      rray_rep_each(x, 2, axis = 1)
    Condition
      Error in `rray_rep_each()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 2
      i Did you forget to name an argument?

# `axis` is validated

    Code
      rray_rep_each(x, times = 2, axis = 0)
    Condition
      Error in `rray_rep_each()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_rep_each(x, times = 2, axis = 3)
    Condition
      Error in `rray_rep_each()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

