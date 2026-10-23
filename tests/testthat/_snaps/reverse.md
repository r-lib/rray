# `axes` is validated

    Code
      rray_reverse(x, axes = TRUE)
    Condition
      Error in `rray_reverse()`:
      ! `axes` must be an integer or double vector, not `TRUE`.

---

    Code
      rray_reverse(x, axes = c(1, 1))
    Condition
      Error in `rray_reverse()`:
      ! `axes` must not contain axis 1 more than once.

---

    Code
      rray_reverse(x, axes = 3)
    Condition
      Error in `rray_reverse()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_reverse(x, axes = 0)
    Condition
      Error in `rray_reverse()`:
      ! `axes` must contain values greater than or equal to 1, not 0.

---

    Code
      rray_reverse(x, axes = NA)
    Condition
      Error in `rray_reverse()`:
      ! `axes` must be an integer or double vector, not `NA`.

---

    Code
      rray_reverse(x, axes = 1.5)
    Condition
      Error in `rray_reverse()`:
      ! `axes` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_reverse(x, axes = "a")
    Condition
      Error in `rray_reverse()`:
      ! `axes` must be an integer or double vector, not the string "a".

# `axes` must be named

    Code
      rray_reverse(1:3, 1)
    Condition
      Error in `rray_reverse()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# errors on invalid input

    Code
      rray_reverse(NULL, axes = 1)
    Condition
      Error in `rray_reverse()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_reverse(x, axes = 1)
    Condition
      Error in `rray_reverse()`:
      ! `x` must be a bare array, not a <foo> object.

