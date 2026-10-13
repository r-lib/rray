# `axis` is validated

    Code
      rray_cumulative_max(x, axis = integer())
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must be a single integer, not length 0.

---

    Code
      rray_cumulative_max(x, axis = 1:2)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_cumulative_max(x, axis = NA_integer_)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must not be missing.

---

    Code
      rray_cumulative_max(x, axis = 0L)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_cumulative_max(x, axis = -1L)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must be greater than or equal to 1, not -1.

---

    Code
      rray_cumulative_max(x, axis = 1.5)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_cumulative_max(x, axis = 3L)
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_cumulative_max(x, axis = structure(1L, foo = 1))
    Condition
      Error in `rray_cumulative_max()`:
      ! `axis` can't have attributes.

---

    Code
      rray_cumulative_min(x, axis = "a")
    Condition
      Error in `rray_cumulative_min()`:
      ! `axis` must be an integer or double vector, not the string "a".

# `axis` must be named

    Code
      rray_cumulative_max(1:3, 1)
    Condition
      Error in `rray_cumulative_max()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

---

    Code
      rray_cumulative_min(1:3, 1)
    Condition
      Error in `rray_cumulative_min()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# errors on unsupported input types

    Code
      rray_cumulative_max(0+1i, axis = 1L)
    Condition
      Error in `rray_cumulative_max()`:
      ! Can't compute the cumulative maximum of `x` <complex>.

---

    Code
      rray_cumulative_min(0+1i, axis = 1L)
    Condition
      Error in `rray_cumulative_min()`:
      ! Can't compute the cumulative minimum of `x` <complex>.

---

    Code
      rray_cumulative_max("a", axis = 1L)
    Condition
      Error in `rray_cumulative_max()`:
      ! Can't compute the cumulative maximum of `x` <character>.

---

    Code
      rray_cumulative_min(as.raw(1L), axis = 1L)
    Condition
      Error in `rray_cumulative_min()`:
      ! Can't compute the cumulative minimum of `x` <raw>.

---

    Code
      rray_cumulative_max(list(1), axis = 1L)
    Condition
      Error in `rray_cumulative_max()`:
      ! Can't compute the cumulative maximum of `x` <list>.

# errors on classed input

    Code
      rray_cumulative_max(x, axis = 1L)
    Condition
      Error in `rray_cumulative_max()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_cumulative_min(x, axis = 1L)
    Condition
      Error in `rray_cumulative_min()`:
      ! `x` must be a bare array, not a <foo> object.

