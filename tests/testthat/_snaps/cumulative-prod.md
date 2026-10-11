# `axis` is validated

    Code
      rray_cumulative_prod(x, integer())
    Condition
      Error in `rray_cumulative_prod()`:
      ! `axis` must be a single integer, not length 0.

---

    Code
      rray_cumulative_prod(x, 1:2)
    Condition
      Error in `rray_cumulative_prod()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_cumulative_prod(x, NA_integer_)
    Condition
      Error in `rray_cumulative_prod()`:
      ! `axis` must not be missing.

---

    Code
      rray_cumulative_prod(x, 0L)
    Condition
      Error in `rray_cumulative_prod()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_cumulative_prod(x, -1L)
    Condition
      Error in `rray_cumulative_prod()`:
      ! `axis` must be greater than or equal to 1, not -1.

---

    Code
      rray_cumulative_prod(x, 1.5)
    Condition
      Error in `rray_cumulative_prod()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_cumulative_prod(x, 3L)
    Condition
      Error in `rray_cumulative_prod()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_cumulative_prod(x, structure(1L, foo = 1))
    Condition
      Error in `rray_cumulative_prod()`:
      ! `axis` can't have attributes.

---

    Code
      rray_cumulative_prod(x, "a")
    Condition
      Error in `rray_cumulative_prod()`:
      ! `axis` must be an integer or double vector, not the string "a".

# errors on unsupported input types

    Code
      rray_cumulative_prod("a", 1L)
    Condition
      Error in `rray_cumulative_prod()`:
      ! Can't compute the cumulative product of `x` <character>.

---

    Code
      rray_cumulative_prod(as.raw(1L), 1L)
    Condition
      Error in `rray_cumulative_prod()`:
      ! Can't compute the cumulative product of `x` <raw>.

---

    Code
      rray_cumulative_prod(list(1), 1L)
    Condition
      Error in `rray_cumulative_prod()`:
      ! Can't compute the cumulative product of `x` <list>.

# errors on classed input

    Code
      rray_cumulative_prod(x, 1L)
    Condition
      Error in `rray_cumulative_prod()`:
      ! `x` must be a bare array, not a <foo> object.

