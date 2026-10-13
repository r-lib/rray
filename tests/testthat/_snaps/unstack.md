# `axis` is validated

    Code
      rray_unstack(x, axis = 0)
    Condition
      Error in `rray_unstack()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_unstack(x, axis = 3)
    Condition
      Error in `rray_unstack()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_unstack(x, axis = c(1, 2))
    Condition
      Error in `rray_unstack()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_unstack(x, axis = NA_integer_)
    Condition
      Error in `rray_unstack()`:
      ! `axis` must not be missing.

---

    Code
      rray_unstack(x, axis = 1.5)
    Condition
      Error in `rray_unstack()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_unstack(x, axis = structure(1L, class = "foo"))
    Condition
      Error in `rray_unstack()`:
      ! `axis` can't have attributes.

# `axis` must be named

    Code
      rray_unstack(array(1:4, c(2, 2)), 1)
    Condition
      Error in `rray_unstack()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# 1D arrays are rejected

    Code
      rray_unstack(array(1:3), axis = 1)
    Condition
      Error in `rray_unstack()`:
      ! `x` must have a dimensionality of at least 2, not 1.

---

    Code
      rray_unstack(1:3, axis = 1)
    Condition
      Error in `rray_unstack()`:
      ! `x` must have a dimensionality of at least 2, not 1.

# errors on invalid input

    Code
      rray_unstack(NULL, axis = 1)
    Condition
      Error in `rray_unstack()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_unstack(x, axis = 1)
    Condition
      Error in `rray_unstack()`:
      ! `x` must be a bare array, not a <foo> object.

