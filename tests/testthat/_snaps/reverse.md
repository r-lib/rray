# `axes` is validated

    Code
      rray_reverse(x, axes = c(1, 1))
    Condition
      Error in `rray_reverse()`:
      ! `axes` must be in strictly increasing order.

---

    Code
      rray_reverse(x, axes = c(2, 1))
    Condition
      Error in `rray_reverse()`:
      ! `axes` must be in strictly increasing order.

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
      ! `axes` must not contain missing values.

---

    Code
      rray_reverse(x, axes = 1.5)
    Condition
      Error:
      ! Can't convert from `axes` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      rray_reverse(x, axes = "a")
    Condition
      Error:
      ! Can't convert `axes` <character> to <integer>.

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

