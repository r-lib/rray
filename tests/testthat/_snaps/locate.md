# `axis` is validated

    Code
      rray_locate_max(x, axis = 0L)
    Condition
      Error in `rray_locate_max()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_locate_max(x, axis = 3L)
    Condition
      Error in `rray_locate_max()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_locate_max(x, axis = c(1L, 2L))
    Condition
      Error in `rray_locate_max()`:
      ! `axis` must be length 1, not length 2.

---

    Code
      rray_locate_max(x, axis = NA_integer_)
    Condition
      Error in `rray_locate_max()`:
      ! `axis` must not be missing.

---

    Code
      rray_locate_min(x, axis = 1.5)
    Condition
      Error in `rray_locate_min()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_locate_min(x, axis = "row")
    Condition
      Error in `rray_locate_min()`:
      ! Can't use a title in `axis` because `x` has no titles.

# errors on an unknown `axis` title

    Code
      rray_locate_max(x, axis = "nope")
    Condition
      Error in `rray_locate_max()`:
      ! Can't find an axis of `x` titled "nope".

# `axis` must be named

    Code
      rray_locate_max(1:3, 1)
    Condition
      Error in `rray_locate_max()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

---

    Code
      rray_locate_min(1:3, 1)
    Condition
      Error in `rray_locate_min()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# `na_rm` must be `TRUE` or `FALSE`

    Code
      rray_locate_max(x, axis = 1L, na_rm = NA)
    Condition
      Error in `rray_locate_max()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_locate_min(x, axis = 1L, na_rm = 1)
    Condition
      Error in `rray_locate_min()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

# errors on unsupported types

    Code
      rray_locate_max(array(0+1i, c(2L, 2L)), axis = 1L)
    Condition
      Error in `rray_locate_max()`:
      ! Can't locate the maximum of `x` <complex>.

---

    Code
      rray_locate_max(array("a", c(2L, 2L)), axis = 1L)
    Condition
      Error in `rray_locate_max()`:
      ! Can't locate the maximum of `x` <character>.

---

    Code
      rray_locate_max(array(as.raw(1:4), c(2L, 2L)), axis = 1L)
    Condition
      Error in `rray_locate_max()`:
      ! Can't locate the maximum of `x` <raw>.

---

    Code
      rray_locate_max(array(list(1, 2, 3, 4), c(2L, 2L)), axis = 1L)
    Condition
      Error in `rray_locate_max()`:
      ! Can't locate the maximum of `x` <list>.

---

    Code
      rray_locate_min(array(0+1i, c(2L, 2L)), axis = 1L)
    Condition
      Error in `rray_locate_min()`:
      ! Can't locate the minimum of `x` <complex>.

# errors on scalar input

    Code
      rray_locate_max(quote(x), axis = 1L)
    Condition
      Error in `rray_locate_max()`:
      ! `x` must be an array, not a symbol.

---

    Code
      rray_locate_min(quote(x), axis = 1L)
    Condition
      Error in `rray_locate_min()`:
      ! `x` must be an array, not a symbol.

# errors on classed input

    Code
      rray_locate_max(x, axis = 1L)
    Condition
      Error in `rray_locate_max()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_locate_min(x, axis = 1L)
    Condition
      Error in `rray_locate_min()`:
      ! `x` must be a bare array, not a <foo> object.

