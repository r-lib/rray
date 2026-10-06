# errors on axes out of range

    Code
      rray_sum(x, 3L)
    Condition
      Error in `rray_sum()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

# errors on axes not in strictly increasing order

    Code
      rray_sum(x, c(1L, 1L))
    Condition
      Error in `rray_sum()`:
      ! `axes` must be in strictly increasing order.

---

    Code
      rray_sum(x, c(2L, 1L))
    Condition
      Error in `rray_sum()`:
      ! `axes` must be in strictly increasing order.

# errors on axes less than 1

    Code
      rray_sum(x, 0L)
    Condition
      Error in `rray_sum()`:
      ! `axes` must contain values greater than or equal to 1, not 0.

# errors on axes with NA

    Code
      rray_sum(x, NA_integer_)
    Condition
      Error in `rray_sum()`:
      ! `axes` must not contain missing values.

# errors on integer overflow

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum_forced_fallback(x, 1L)
    Condition
      Error in `rray_sum_forced_fallback()`:
      ! Integer overflow.

# errors on integer underflow

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum_forced_fallback(x, 1L)
    Condition
      Error in `rray_sum_forced_fallback()`:
      ! Integer overflow.

# errors on integer overflow with `na_rm = TRUE`

    Code
      rray_sum(x, 1L, na_rm = TRUE)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum_forced_fallback(x, 1L, na_rm = TRUE)
    Condition
      Error in `rray_sum_forced_fallback()`:
      ! Integer overflow.

# errors on integer overflow along axis 2

    Code
      rray_sum(x, 2L)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum_forced_fallback(x, 2L)
    Condition
      Error in `rray_sum_forced_fallback()`:
      ! Integer overflow.

---

    Code
      rray_sum(x, 2L, na_rm = TRUE)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum_forced_fallback(x, 2L, na_rm = TRUE)
    Condition
      Error in `rray_sum_forced_fallback()`:
      ! Integer overflow.

# errors when a logical sum has too many `TRUE` values

    Code
      rray_sum(x, 1:2)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

# integer sums past 2^32 elements fall back and error on overflow

    Code
      rray_sum(x, 1:2)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

---

    Code
      rray_sum(x, 1:2, na_rm = TRUE)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow.

# `na_rm` must be `TRUE` or `FALSE`

    Code
      rray_sum(x, 1L, na_rm = NA)
    Condition
      Error in `rray_sum()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_sum(x, 1L, na_rm = logical())
    Condition
      Error in `rray_sum()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_sum(x, 1L, na_rm = c(TRUE, FALSE))
    Condition
      Error in `rray_sum()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_sum(x, 1L, na_rm = 1)
    Condition
      Error in `rray_sum()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

# errors on non-numeric input

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! Can't compute the sum of `x` <character>.

---

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! Can't compute the sum of `x` <raw>.

---

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! Can't compute the sum of `x` <list>.

# errors on classed input

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! `x` must be a bare array, not a <foo> object.

