# errors on integer overflow

    Code
      rray_cumulative_sum(x, axis = 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Integer overflow.

---

    Code
      rray_cumulative_sum(x, axis = 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Integer overflow.

---

    Code
      rray_cumulative_sum(x, axis = 2L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Integer overflow.

# errors on integer overflow even if a later value cancels it

    Code
      rray_cumulative_sum(x, axis = 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Integer overflow.

# `axis` is validated

    Code
      rray_cumulative_sum(x, axis = integer())
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be length 1, not length 0.

---

    Code
      rray_cumulative_sum(x, axis = 1:2)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be length 1, not length 2.

---

    Code
      rray_cumulative_sum(x, axis = NA_integer_)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must not be missing.

---

    Code
      rray_cumulative_sum(x, axis = 0L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_cumulative_sum(x, axis = -1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be greater than or equal to 1, not -1.

---

    Code
      rray_cumulative_sum(x, axis = 1.5)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_cumulative_sum(x, axis = 3L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_cumulative_sum(x, axis = structure(1L, foo = 1))
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` can't have attributes.

---

    Code
      rray_cumulative_sum(x, axis = "a")
    Condition
      Error in `rray_cumulative_sum()`:
      ! Can't use a title in `axis` because `x` has no titles.

---

    Code
      rray_cumulative_sum(x, axis = list(1))
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be an integer, double, or character vector, not a list.

# `axis` titles are validated

    Code
      rray_cumulative_sum(x, axis = "nope")
    Condition
      Error in `rray_cumulative_sum()`:
      ! Can't find an axis of `x` titled "nope".

---

    Code
      rray_cumulative_sum(x, axis = NA_character_)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must not be missing.

---

    Code
      rray_cumulative_sum(x, axis = "")
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` can't be an empty string.

---

    Code
      rray_cumulative_sum(x, axis = c("row", "col"))
    Condition
      Error in `rray_cumulative_sum()`:
      ! `axis` must be length 1, not length 2.

# `axis` must be named

    Code
      rray_cumulative_sum(1:3, 1)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# errors on unsupported input types

    Code
      rray_cumulative_sum("a", axis = 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Can't compute the cumulative sum of `x` <character>.

---

    Code
      rray_cumulative_sum(as.raw(1L), axis = 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Can't compute the cumulative sum of `x` <raw>.

---

    Code
      rray_cumulative_sum(list(1), axis = 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! Can't compute the cumulative sum of `x` <list>.

# errors on classed input

    Code
      rray_cumulative_sum(x, axis = 1L)
    Condition
      Error in `rray_cumulative_sum()`:
      ! `x` must be a bare array, not a <foo> object.

