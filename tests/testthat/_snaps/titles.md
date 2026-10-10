# title getters reject invalid input and axes

    Code
      rray_titles(NULL)
    Condition
      Error in `rray_titles()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_titles(classed)
    Condition
      Error in `rray_titles()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_axis_title(x, 0)
    Condition
      Error in `rray_axis_title()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_axis_title(x, 3)
    Condition
      Error in `rray_axis_title()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_axis_title(x, NA_integer_)
    Condition
      Error in `rray_axis_title()`:
      ! `axis` must not be missing.

---

    Code
      rray_axis_title(x, c(1, 2))
    Condition
      Error in `rray_axis_title()`:
      ! `axis` must be a single integer, not length 2.

# title setters reject invalid input and values

    Code
      rray_set_titles(classed, NULL)
    Condition
      Error in `rray_set_titles()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_set_titles(x, 1:2)
    Condition
      Error in `rray_set_titles()`:
      ! `titles` must be a character vector or `NULL`, not an integer vector.

---

    Code
      rray_set_titles(x, "Row")
    Condition
      Error in `rray_set_titles()`:
      ! `titles` must have length 2 to match the dimensionality of `x`, not length 1.

---

    Code
      rray_set_axis_title(x, 0, "Row")
    Condition
      Error in `rray_set_axis_title()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_set_axis_title(x, 1, 1L)
    Condition
      Error in `rray_set_axis_title()`:
      ! `title` must be a character vector or `NULL`, not the number 1.

---

    Code
      rray_set_axis_title(x, 1, character())
    Condition
      Error in `rray_set_axis_title()`:
      ! `title` must have length 1, not length 0.

---

    Code
      rray_set_axis_title(x, 1, c("Row", "Column"))
    Condition
      Error in `rray_set_axis_title()`:
      ! `title` must have length 1, not length 2.
