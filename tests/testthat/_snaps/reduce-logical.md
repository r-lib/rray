# errors on axes out of range

    Code
      rray_all_along(x, 3L)
    Condition
      Error in `rray_all_along()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_any_along(x, 3L)
    Condition
      Error in `rray_any_along()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

# `na_rm` must be `TRUE` or `FALSE`

    Code
      rray_all_along(x, 1L, na_rm = NA)
    Condition
      Error in `rray_all_along()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_any_along(x, 1L, na_rm = 1)
    Condition
      Error in `rray_any_along()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

# errors on non-logical input

    Code
      rray_all_along(array(1L, c(2L, 2L)), 1L)
    Condition
      Error in `rray_all_along()`:
      ! `x` must be a logical array, not an integer matrix.

---

    Code
      rray_all_along(array(1, c(2L, 2L)), 1L)
    Condition
      Error in `rray_all_along()`:
      ! `x` must be a logical array, not a double matrix.

---

    Code
      rray_all_along(array(0+1i, c(2L, 2L)), 1L)
    Condition
      Error in `rray_all_along()`:
      ! `x` must be a logical array, not a complex matrix.

---

    Code
      rray_all_along(array("a", c(2L, 2L)), 1L)
    Condition
      Error in `rray_all_along()`:
      ! `x` must be a logical array, not a character matrix.

---

    Code
      rray_all_along(array(as.raw(1:4), c(2L, 2L)), 1L)
    Condition
      Error in `rray_all_along()`:
      ! `x` must be a logical array, not a raw matrix.

---

    Code
      rray_all_along(array(list(1, 2, 3, 4), c(2L, 2L)), 1L)
    Condition
      Error in `rray_all_along()`:
      ! `x` must be a logical array, not a list matrix.

---

    Code
      rray_any_along(array(1L, c(2L, 2L)), 1L)
    Condition
      Error in `rray_any_along()`:
      ! `x` must be a logical array, not an integer matrix.

# errors on scalar input

    Code
      rray_all_along(quote(x), 1L)
    Condition
      Error in `rray_all_along()`:
      ! `x` must be an array, not a symbol.

---

    Code
      rray_any_along(quote(x), 1L)
    Condition
      Error in `rray_any_along()`:
      ! `x` must be an array, not a symbol.

# errors on classed input

    Code
      rray_all_along(x, 1L)
    Condition
      Error in `rray_all_along()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_any_along(x, 1L)
    Condition
      Error in `rray_any_along()`:
      ! `x` must be a bare array, not a <foo> object.

