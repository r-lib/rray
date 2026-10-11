# `axis` is validated

    Code
      rray_cumulative_all(x, integer())
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must be a single integer, not length 0.

---

    Code
      rray_cumulative_all(x, 1:2)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_cumulative_all(x, NA_integer_)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must not be missing.

---

    Code
      rray_cumulative_all(x, 0L)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_cumulative_all(x, -1L)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must be greater than or equal to 1, not -1.

---

    Code
      rray_cumulative_all(x, 1.5)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_cumulative_all(x, 3L)
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_cumulative_all(x, structure(1L, foo = 1))
    Condition
      Error in `rray_cumulative_all()`:
      ! `axis` can't have attributes.

---

    Code
      rray_cumulative_any(x, "a")
    Condition
      Error in `rray_cumulative_any()`:
      ! `axis` must be an integer or double vector, not the string "a".

# errors on non-logical input

    Code
      rray_cumulative_all(1:2, 1L)
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
      rray_cumulative_all(x, 1L)
    Condition
      Error in `rray_cumulative_all()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_cumulative_any(x, 1L)
    Condition
      Error in `rray_cumulative_any()`:
      ! `x` must be a bare array, not a <foo> object.

