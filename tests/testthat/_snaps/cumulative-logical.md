# `axis` is validated

    Code
      rray_cumulative_all(x, axis = integer())
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must be a single integer, not length 0.

---

    Code
      rray_cumulative_all(x, axis = 1:2)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_cumulative_all(x, axis = NA_integer_)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must not be missing.

---

    Code
      rray_cumulative_all(x, axis = 0L)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_cumulative_all(x, axis = -1L)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must be greater than or equal to 1, not -1.

---

    Code
      rray_cumulative_all(x, axis = 1.5)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_cumulative_all(x, axis = 3L)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_cumulative_all(x, axis = structure(1L, foo = 1))
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` can't have attributes.

---

    Code
      rray_cumulative_any(x, "a")
    Condition
      Error in `rray_cumulative_any()`:
      ! `axis` must be an integer or double vector, not the string "a".

# `axis` must be named

    Code
      rray_cumulative_all(c(TRUE, FALSE), 1)
    Condition
      Error in `rray_cumulative_all()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# errors on non-logical input

    Code
      rray_cumulative_all(1:2, axis = 1L)
    Condition
      Error in `rray_cumulative_all()`:
      ! `x` must be a logical array, not an integer 1D array.

---

    Code
      rray_cumulative_any(array(1, c(2L, 2L)), 1L)
    Condition
      Error in `rray_cumulative_any()`:
      ! `x` must be a logical array, not a double matrix.

# errors on classed input

    Code
      rray_cumulative_all(x, axis = 1L)
    Condition
      Error in `rray_cumulative_all()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_cumulative_any(x, 1L)
    Condition
      Error in `rray_cumulative_any()`:
      ! `x` must be a bare array, not a <foo> object.

