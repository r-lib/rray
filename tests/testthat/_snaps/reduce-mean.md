# errors on axes out of range

    Code
      rray_mean(x, 3L)
    Condition
      Error in `rray_mean()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

# errors on axes not in strictly increasing order

    Code
      rray_mean(x, c(1L, 1L))
    Condition
      Error in `rray_mean()`:
      ! `axes` must be in strictly increasing order.

---

    Code
      rray_mean(x, c(2L, 1L))
    Condition
      Error in `rray_mean()`:
      ! `axes` must be in strictly increasing order.

# errors on axes less than 1

    Code
      rray_mean(x, 0L)
    Condition
      Error in `rray_mean()`:
      ! `axes` must contain values greater than or equal to 1, not 0.

# errors on axes with NA

    Code
      rray_mean(x, NA_integer_)
    Condition
      Error in `rray_mean()`:
      ! `axes` must not contain missing values.

# `na_rm` must be `TRUE` or `FALSE`

    Code
      rray_mean(x, 1L, na_rm = NA)
    Condition
      Error in `rray_mean()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_mean(x, 1L, na_rm = logical())
    Condition
      Error in `rray_mean()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_mean(x, 1L, na_rm = c(TRUE, FALSE))
    Condition
      Error in `rray_mean()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_mean(x, 1L, na_rm = 1)
    Condition
      Error in `rray_mean()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

# errors on complex input

    Code
      rray_mean(x, 1L)
    Condition
      Error in `rray_mean()`:
      ! Can't compute the mean of `x` <complex>.

# errors on non-numeric input

    Code
      rray_mean(x, 1L)
    Condition
      Error in `rray_mean()`:
      ! Can't compute the mean of `x` <character>.

---

    Code
      rray_mean(x, 1L)
    Condition
      Error in `rray_mean()`:
      ! Can't compute the mean of `x` <raw>.

---

    Code
      rray_mean(x, 1L)
    Condition
      Error in `rray_mean()`:
      ! Can't compute the mean of `x` <list>.

# errors on classed input

    Code
      rray_mean(x, 1L)
    Condition
      Error in `rray_mean()`:
      ! `x` must be a bare array, not a <foo> object.

