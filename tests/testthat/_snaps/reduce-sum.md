# errors on axes out of range

    Code
      rray_sum(x, axes = 3L)
    Condition
      Error in `rray_sum()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

# errors on duplicate axes

    Code
      rray_sum(x, axes = c(1L, 1L))
    Condition
      Error in `rray_sum()`:
      ! `axes` must not contain axis 1 more than once.

---

    Code
      rray_sum(x, axes = c(2L, 1L, 2L))
    Condition
      Error in `rray_sum()`:
      ! `axes` must not contain axis 2 more than once.

# errors on axes less than 1

    Code
      rray_sum(x, axes = 0L)
    Condition
      Error in `rray_sum()`:
      ! `axes` must contain values greater than or equal to 1, not 0.

# errors on axes with NA

    Code
      rray_sum(x, axes = NA_integer_)
    Condition
      Error in `rray_sum()`:
      ! `axes` must not contain missing values.

# errors on titles that aren't found

    Code
      rray_sum(x, axes = "nope")
    Condition
      Error in `rray_sum()`:
      ! Can't find an axis of `x` titled "nope".

---

    Code
      rray_sum(x, axes = c("row", "nope"))
    Condition
      Error in `rray_sum()`:
      ! Can't find an axis of `x` titled "nope".

# errors on titles when `x` has no titles

    Code
      rray_sum(x, axes = "row")
    Condition
      Error in `rray_sum()`:
      ! Can't use a title in `axes` because `x` has no titles.

# errors on missing or empty titles

    Code
      rray_sum(x, axes = c("row", NA))
    Condition
      Error in `rray_sum()`:
      ! `axes` must not contain missing values.

---

    Code
      rray_sum(x, axes = "")
    Condition
      Error in `rray_sum()`:
      ! `axes` can't contain empty strings.

# errors on duplicate titles

    Code
      rray_sum(x, axes = c("row", "row"))
    Condition
      Error in `rray_sum()`:
      ! `axes` must not contain "row" more than once.

# errors on axes of the wrong type

    Code
      rray_sum(x, axes = list(1))
    Condition
      Error in `rray_sum()`:
      ! `axes` must be an integer, double, or character vector, not a list.

---

    Code
      rray_sum(x, axes = factor("row"))
    Condition
      Error in `rray_sum()`:
      ! `axes` can't have attributes.

# errors on integer overflow

    Code
      rray_sum(x, axes = 1L)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum_forced_fallback(x, axes = 1L)
    Condition
      Error in `rray_sum_forced_fallback()`:
      ! Integer overflow.

# errors on integer underflow

    Code
      rray_sum(x, axes = 1L)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum_forced_fallback(x, axes = 1L)
    Condition
      Error in `rray_sum_forced_fallback()`:
      ! Integer overflow.

# errors on integer overflow with `na_rm = TRUE`

    Code
      rray_sum(x, axes = 1L, na_rm = TRUE)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum_forced_fallback(x, axes = 1L, na_rm = TRUE)
    Condition
      Error in `rray_sum_forced_fallback()`:
      ! Integer overflow.

# errors on integer overflow along axis 2

    Code
      rray_sum(x, axes = 2L)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum_forced_fallback(x, axes = 2L)
    Condition
      Error in `rray_sum_forced_fallback()`:
      ! Integer overflow.

---

    Code
      rray_sum(x, axes = 2L, na_rm = TRUE)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum_forced_fallback(x, axes = 2L, na_rm = TRUE)
    Condition
      Error in `rray_sum_forced_fallback()`:
      ! Integer overflow.

# errors when a logical sum has too many `TRUE` values

    Code
      rray_sum(x, axes = 1:2)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

# integer sums past 2^32 elements fall back and error on overflow

    Code
      rray_sum(x, axes = 1:2)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum(x, axes = 1:2, na_rm = TRUE)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

# `axes` must be named

    Code
      rray_sum(1:3, 1)
    Condition
      Error in `rray_sum()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# `na_rm` must be `TRUE` or `FALSE`

    Code
      rray_sum(x, axes = 1L, na_rm = NA)
    Condition
      Error in `rray_sum()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_sum(x, axes = 1L, na_rm = logical())
    Condition
      Error in `rray_sum()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_sum(x, axes = 1L, na_rm = c(TRUE, FALSE))
    Condition
      Error in `rray_sum()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_sum(x, axes = 1L, na_rm = 1)
    Condition
      Error in `rray_sum()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

# errors on non-numeric input

    Code
      rray_sum(x, axes = 1L)
    Condition
      Error in `rray_sum()`:
      ! Can't compute the sum of `x` <character>.

---

    Code
      rray_sum(x, axes = 1L)
    Condition
      Error in `rray_sum()`:
      ! Can't compute the sum of `x` <raw>.

---

    Code
      rray_sum(x, axes = 1L)
    Condition
      Error in `rray_sum()`:
      ! Can't compute the sum of `x` <list>.

# errors on classed input

    Code
      rray_sum(x, axes = 1L)
    Condition
      Error in `rray_sum()`:
      ! `x` must be a bare array, not a <foo> object.

