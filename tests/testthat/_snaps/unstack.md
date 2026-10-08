# `axis` is validated

    Code
      rray_unstack(x, 0)
    Condition
      Error in `rray_unstack()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_unstack(x, 3)
    Condition
      Error in `rray_unstack()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_unstack(x, c(1, 2))
    Condition
      Error in `rray_unstack()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_unstack(x, NA_integer_)
    Condition
      Error in `rray_unstack()`:
      ! `axis` must not be missing.

---

    Code
      rray_unstack(x, 1.5)
    Condition
      Error in `rray_unstack()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_unstack(x, structure(1L, class = "foo"))
    Condition
      Error in `rray_unstack()`:
      ! `axis` can't have attributes.

# 1D arrays are rejected

    Code
      rray_unstack(array(1:3), 1)
    Condition
      Error in `rray_unstack()`:
      ! `x` must have a dimensionality of at least 2, not 1.

---

    Code
      rray_unstack(1:3, 1)
    Condition
      Error in `rray_unstack()`:
      ! `x` must have a dimensionality of at least 2, not 1.

# errors on invalid input

    Code
      rray_unstack(NULL, 1)
    Condition
      Error in `rray_unstack()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_unstack(x, 1)
    Condition
      Error in `rray_unstack()`:
      ! `x` must be a bare array, not a <foo> object.

