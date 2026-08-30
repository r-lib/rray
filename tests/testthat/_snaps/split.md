# axes are validated

    Code
      rray_split(x, 3)
    Condition
      Error in `rray_split()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_split(x, 0)
    Condition
      Error in `rray_split()`:
      ! `axes` must contain values greater than or equal to 1, not 0.

---

    Code
      rray_split(x, c(1, 1))
    Condition
      Error in `rray_split()`:
      ! `axes` must be in strictly increasing order.

---

    Code
      rray_split(x, c(2, 1))
    Condition
      Error in `rray_split()`:
      ! `axes` must be in strictly increasing order.

# errors on classed input

    Code
      rray_split(x, 1L)
    Condition
      Error in `rray_split()`:
      ! `x` must be a bare array, not a <foo> object.

