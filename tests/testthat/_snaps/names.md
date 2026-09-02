# errors on non-vector types

    Code
      rray_names(NULL)
    Condition
      Error in `rray_names()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_names(mean)
    Condition
      Error in `rray_names()`:
      ! `x` must be an array, not a function.

# errors on classed input

    Code
      rray_names(x)
    Condition
      Error in `rray_names()`:
      ! `x` must be a bare array, not a <foo> object.

# rray_axis_names() errors on an invalid axis

    Code
      rray_axis_names(x, 0)
    Condition
      Error in `rray_axis_names()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_axis_names(x, 3)
    Condition
      Error in `rray_axis_names()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_axis_names(x, NA_integer_)
    Condition
      Error in `rray_axis_names()`:
      ! `axis` must not be missing.

---

    Code
      rray_axis_names(x, c(1, 2))
    Condition
      Error in `rray_axis_names()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_axis_names(x, integer())
    Condition
      Error in `rray_axis_names()`:
      ! `axis` must be a single integer, not length 0.

# rray_axis_names() errors on classed input

    Code
      rray_axis_names(x, 1)
    Condition
      Error in `rray_axis_names()`:
      ! `x` must be a bare array, not a <foo> object.

# rray_col_names() errors if `x` doesn't have a second axis

    Code
      rray_col_names(1:5)
    Condition
      Error in `rray_col_names()`:
      ! `axis` must be less than or equal to the dimensionality of 1, not 2.

# rray_row_names() and rray_col_names() error on classed input

    Code
      rray_row_names(x)
    Condition
      Error in `rray_row_names()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_col_names(x)
    Condition
      Error in `rray_col_names()`:
      ! `x` must be a bare array, not a <foo> object.

# rray_set_names() errors if `names` isn't a list or `NULL`

    Code
      rray_set_names(x, c("r1", "r2"))
    Condition
      Error in `rray_set_names()`:
      ! `names` must be a list or `NULL`, not a character vector.

# rray_set_names() errors if length doesn't match dimensionality

    Code
      rray_set_names(x, list(c("r1", "r2")))
    Condition
      Error in `rray_set_names()`:
      ! `names` must have length 2 to match the dimensionality of `x`, not length 1.

---

    Code
      rray_set_names(x, list(c("r1", "r2"), NULL, NULL))
    Condition
      Error in `rray_set_names()`:
      ! `names` must have length 2 to match the dimensionality of `x`, not length 3.

# rray_set_names() errors if an axis' names aren't character

    Code
      rray_set_names(x, list(1:2, NULL))
    Condition
      Error in `rray_set_names()`:
      ! Names for axis 1 must be a character vector or `NULL`, not an integer vector.

# rray_set_names() errors if an axis' names are the wrong length

    Code
      rray_set_names(x, list("r1", NULL))
    Condition
      Error in `rray_set_names()`:
      ! Names for axis 1 must have length 2, not length 1.

# rray_set_names() errors on classed input

    Code
      rray_set_names(x, NULL)
    Condition
      Error in `rray_set_names()`:
      ! `x` must be a bare array, not a <foo> object.

# rray_set_axis_names() errors on an invalid axis

    Code
      rray_set_axis_names(x, 0, "a")
    Condition
      Error in `rray_set_axis_names()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_set_axis_names(x, 3, "a")
    Condition
      Error in `rray_set_axis_names()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

# rray_set_axis_names() errors if names aren't character or NULL

    Code
      rray_set_axis_names(x, 1, 1:2)
    Condition
      Error in `rray_set_axis_names()`:
      ! Names for axis 1 must be a character vector or `NULL`, not an integer vector.

# rray_set_axis_names() errors if names are the wrong length

    Code
      rray_set_axis_names(x, 1, "r1")
    Condition
      Error in `rray_set_axis_names()`:
      ! Names for axis 1 must have length 2, not length 1.

# rray_set_axis_names() errors on classed input

    Code
      rray_set_axis_names(x, 1, NULL)
    Condition
      Error in `rray_set_axis_names()`:
      ! `x` must be a bare array, not a <foo> object.

# rray_set_row_names() and rray_set_col_names() error on classed input

    Code
      rray_set_row_names(x, NULL)
    Condition
      Error in `rray_set_row_names()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_set_col_names(x, NULL)
    Condition
      Error in `rray_set_col_names()`:
      ! `x` must be a bare array, not a <foo> object.

