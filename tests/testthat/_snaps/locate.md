# `axis` is validated

    Code
      rray_locate_max(x, 0L)
    Condition
      Error in `rray_locate_max()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_locate_max(x, 3L)
    Condition
      Error in `rray_locate_max()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_locate_max(x, c(1L, 2L))
    Condition
      Error in `rray_locate_max()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_locate_max(x, NA_integer_)
    Condition
      Error in `rray_locate_max()`:
      ! `axis` must not be missing.

---

    Code
      rray_locate_min(x, 1.5)
    Condition
      Error:
      ! Can't convert from `axis` <double> to <integer> due to loss of precision.
      * Locations: 1

# `na_rm` must be `TRUE` or `FALSE`

    Code
      rray_locate_max(x, 1L, na_rm = NA)
    Condition
      Error in `rray_locate_max()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_locate_min(x, 1L, na_rm = 1)
    Condition
      Error in `rray_locate_min()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

# errors on unsupported types

    Code
      rray_locate_max(array(0+1i, c(2L, 2L)), 1L)
    Condition
      Error in `rray_locate_max()`:
      ! Can't locate the maximum of `x` <complex>.

---

    Code
      rray_locate_max(array("a", c(2L, 2L)), 1L)
    Condition
      Error in `rray_locate_max()`:
      ! Can't locate the maximum of `x` <character>.

---

    Code
      rray_locate_max(array(as.raw(1:4), c(2L, 2L)), 1L)
    Condition
      Error in `rray_locate_max()`:
      ! Can't locate the maximum of `x` <raw>.

---

    Code
      rray_locate_max(array(list(1, 2, 3, 4), c(2L, 2L)), 1L)
    Condition
      Error in `rray_locate_max()`:
      ! Can't locate the maximum of `x` <list>.

---

    Code
      rray_locate_min(array(0+1i, c(2L, 2L)), 1L)
    Condition
      Error in `rray_locate_min()`:
      ! Can't locate the minimum of `x` <complex>.

# errors on scalar input

    Code
      rray_locate_max(quote(x), 1L)
    Condition
      Error in `rray_locate_max()`:
      ! `x` must be an array, not a symbol.

---

    Code
      rray_locate_min(quote(x), 1L)
    Condition
      Error in `rray_locate_min()`:
      ! `x` must be an array, not a symbol.

# errors on classed input

    Code
      rray_locate_max(x, 1L)
    Condition
      Error in `rray_locate_max()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_locate_min(x, 1L)
    Condition
      Error in `rray_locate_min()`:
      ! `x` must be a bare array, not a <foo> object.

