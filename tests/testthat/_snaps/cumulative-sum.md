# errors on integer overflow

    Code
      rray_cumulative_sum(x, 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Integer overflow.

---

    Code
      rray_cumulative_sum(x, 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Integer overflow.

---

    Code
      rray_cumulative_sum(x, 2L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Integer overflow.

# errors on integer overflow even if a later value cancels it

    Code
      rray_cumulative_sum(x, 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Integer overflow.

# `axis` is validated

    Code
      rray_cumulative_sum(x, integer())
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be a single integer, not length 0.

---

    Code
      rray_cumulative_sum(x, 1:2)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_cumulative_sum(x, NA_integer_)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must not be missing.

---

    Code
      rray_cumulative_sum(x, 0L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_cumulative_sum(x, -1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be greater than or equal to 1, not -1.

---

    Code
      rray_cumulative_sum(x, 1.5)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_cumulative_sum(x, 3L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_cumulative_sum(x, structure(1L, foo = 1))
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` can't have attributes.

---

    Code
      rray_cumulative_sum(x, "a")
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be an integer or double vector, not the string "a".

# errors on unsupported input types

    Code
      rray_cumulative_sum("a", 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Can't compute the cumulative sum of `x` <character>.

---

    Code
      rray_cumulative_sum(as.raw(1L), 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Can't compute the cumulative sum of `x` <raw>.

---

    Code
      rray_cumulative_sum(list(1), 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Can't compute the cumulative sum of `x` <list>.

# errors on classed input

    Code
      rray_cumulative_sum(x, 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `x` must be a bare array, not a <foo> object.

