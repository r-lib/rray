# errors on axes out of range

    Code
      rray_max(x, 3L)
    Condition
      Error in `rray_max()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_min(x, 3L)
    Condition
      Error in `rray_min()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

# `na_rm` must be `TRUE` or `FALSE`

    Code
      rray_max(x, 1L, na_rm = NA)
    Condition
      Error in `rray_max()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_min(x, 1L, na_rm = 1)
    Condition
      Error in `rray_min()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

# errors on unsupported types

    Code
      rray_max(array(0+1i, c(2L, 2L)), 1L)
    Condition
      Error in `rray_max()`:
      ! Can't compute the maximum of `x` <complex>.

---

    Code
      rray_max(array("a", c(2L, 2L)), 1L)
    Condition
      Error in `rray_max()`:
      ! Can't compute the maximum of `x` <character>.

---

    Code
      rray_max(array(as.raw(1:4), c(2L, 2L)), 1L)
    Condition
      Error in `rray_max()`:
      ! Can't compute the maximum of `x` <raw>.

---

    Code
      rray_max(array(list(1, 2, 3, 4), c(2L, 2L)), 1L)
    Condition
      Error in `rray_max()`:
      ! Can't compute the maximum of `x` <list>.

---

    Code
      rray_min(array(0+1i, c(2L, 2L)), 1L)
    Condition
      Error in `rray_min()`:
      ! Can't compute the minimum of `x` <complex>.

# errors on scalar input

    Code
      rray_max(quote(x), 1L)
    Condition
      Error in `rray_max()`:
      ! `x` must be an array, not a symbol.

---

    Code
      rray_min(quote(x), 1L)
    Condition
      Error in `rray_min()`:
      ! `x` must be an array, not a symbol.

# errors on classed input

    Code
      rray_max(x, 1L)
    Condition
      Error in `rray_max()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_min(x, 1L)
    Condition
      Error in `rray_min()`:
      ! `x` must be a bare array, not a <foo> object.

