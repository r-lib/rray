# `times` and `axis` must be named

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
      rray_rep(x, 2, axis = 1)
    Condition
      Error in `rray_rep()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 2
      i Did you forget to name an argument?

---

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
      rray_rep(x, times = 2, axis = 0)
    Condition
      Error in `rray_rep()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_rep(x, times = 2, axis = 3)
    Condition
      Error in `rray_rep()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_rep(x, times = 2, axis = c(1, 2))
    Condition
      Error in `rray_rep()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_rep(x, times = 2, axis = NA_integer_)
    Condition
      Error in `rray_rep()`:
      ! `axis` must not be missing.

---

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

# `times` is validated

    Code
      rray_rep(x, times = NA_integer_, axis = 1)
    Condition
      Error in `rray_rep()`:
      ! `times` must not contain missing values.

---

    Code
      rray_rep(x, times = -1, axis = 1)
    Condition
      Error in `rray_rep()`:
      ! `times` must not contain negative values.

---

    Code
      rray_rep(x, times = 1.5, axis = 1)
    Condition
      Error:
      ! Can't convert from `times` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      rray_rep(x, times = structure(1L, names = "a"), axis = 1)
    Condition
      Error in `rray_rep()`:
      ! `times` can't have attributes.

---

    Code
      rray_rep(x, times = c(1, 2), axis = 1)
    Condition
      Error in `rray_rep()`:
      ! `times` must be size 1, not size 2.

---

    Code
      rray_rep(x, times = integer(), axis = 1)
    Condition
      Error in `rray_rep()`:
      ! `times` must be size 1, not size 0.

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
      rray_rep(x, times = .Machine$integer.max, axis = 1)
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

# errors on invalid input

    Code
      rray_rep(NULL, times = 2, axis = 1)
    Condition
      Error in `rray_rep()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_rep(x, times = 2, axis = 1)
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

