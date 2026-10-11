# `axis` is validated

    Code
      rray_cumulative_max(x, integer())
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must be a single integer, not length 0.

---

    Code
      rray_cumulative_max(x, 1:2)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_cumulative_max(x, NA_integer_)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must not be missing.

---

    Code
      rray_cumulative_max(x, 0L)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_cumulative_max(x, -1L)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must be greater than or equal to 1, not -1.

---

    Code
      rray_cumulative_max(x, 1.5)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_cumulative_max(x, 3L)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_cumulative_max(x, structure(1L, foo = 1))
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` can't have attributes.

---

    Code
      rray_cumulative_min(x, "a")
    Condition
      Error in `rray_cumulative_min()`:
      ! `axis` must be an integer or double vector, not the string "a".

# errors on unsupported input types

    Code
      rray_cumulative_max(0+1i, 1L)
    Condition
      Error in `rray_cumulative_max()`:
      ! Can't compute the cumulative maximum of `x` <complex>.

---

    Code
      rray_cumulative_min(0+1i, 1L)
    Condition
      Error in `rray_cumulative_min()`:
      ! Can't compute the cumulative minimum of `x` <complex>.

---

    Code
      rray_cumulative_max("a", 1L)
    Condition
      Error in `rray_cumulative_max()`:
      ! Can't compute the cumulative maximum of `x` <character>.

---

    Code
      rray_cumulative_min(as.raw(1L), 1L)
    Condition
      Error in `rray_cumulative_min()`:
      ! Can't compute the cumulative minimum of `x` <raw>.

---

    Code
      rray_cumulative_max(list(1), 1L)
    Condition
      Error in `rray_cumulative_max()`:
      ! Can't compute the cumulative maximum of `x` <list>.

# errors on classed input

    Code
      rray_cumulative_max(x, 1L)
    Condition
      Error in `rray_cumulative_max()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_cumulative_min(x, 1L)
    Condition
      Error in `rray_cumulative_min()`:
      ! `x` must be a bare array, not a <foo> object.

