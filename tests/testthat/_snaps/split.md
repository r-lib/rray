# `axis` and `dimensions` must be named

    Code
      rray_split(x, 1, 1)
    Condition
      Error in `rray_split()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 1
      * ..2 = 1
      i Did you forget to name an argument?
    Code
      rray_split(x, 1, dimensions = 1)
    Condition
      Error in `rray_split()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# `axis` is validated

    Code
      rray_split(x, axis = 0, dimensions = 1)
    Condition
      Error in `rray_split()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_split(x, axis = 3, dimensions = 1)
    Condition
      Error in `rray_split()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_split(x, axis = c(1, 2), dimensions = 1)
    Condition
      Error in `rray_split()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_split(x, axis = NA_integer_, dimensions = 1)
    Condition
      Error in `rray_split()`:
      ! `axis` must not be missing.

---

    Code
      rray_split(x, axis = 1.5, dimensions = 1)
    Condition
      Error in `rray_split()`:
      ! `axis` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_split(x, axis = structure(1L, class = "foo"), dimensions = 1)
    Condition
      Error in `rray_split()`:
      ! `axis` can't have attributes.

# `dimensions` are validated

    Code
      rray_split(x, axis = 1, dimensions = NA_integer_)
    Condition
      Error in `rray_split()`:
      ! `dimensions` must not contain missing values.

---

    Code
      rray_split(x, axis = 1, dimensions = 1.5)
    Condition
      Error in `rray_split()`:
      ! `dimensions` must contain whole numbers that fit in an integer. Problem at location 1.

---

    Code
      rray_split(x, axis = 1, dimensions = structure(1L, names = "a"))
    Condition
      Error in `rray_split()`:
      ! `dimensions` can't have attributes.

---

    Code
      rray_split(x, axis = 1, dimensions = 0)
    Condition
      Error in `rray_split()`:
      ! A single `dimensions` value must be positive, not 0.

---

    Code
      rray_split(x, axis = 1, dimensions = -1)
    Condition
      Error in `rray_split()`:
      ! `dimensions` must not contain negative values.

---

    Code
      rray_split(x, axis = 2, dimensions = 2)
    Condition
      Error in `rray_split()`:
      ! A single `dimensions` value of 2 must evenly divide the `axis` dimension of 3.

---

    Code
      rray_split(x, axis = 1, dimensions = c(1, -1))
    Condition
      Error in `rray_split()`:
      ! `dimensions` must not contain negative values.

---

    Code
      rray_split(x, axis = 1, dimensions = c(1, 2))
    Condition
      Error in `rray_split()`:
      ! `dimensions` must sum to the `axis` dimension of 2, not 3.

---

    Code
      rray_split(x, axis = 2, dimensions = c(1, 1))
    Condition
      Error in `rray_split()`:
      ! `dimensions` must sum to the `axis` dimension of 3, not 2.

---

    Code
      rray_split(x, axis = 1, dimensions = integer())
    Condition
      Error in `rray_split()`:
      ! `dimensions` must sum to the `axis` dimension of 2, not 0.

# errors on invalid input

    Code
      rray_split(NULL, axis = 1, dimensions = 1)
    Condition
      Error in `rray_split()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_split(x, axis = 1, dimensions = 1)
    Condition
      Error in `rray_split()`:
      ! `x` must be a bare array, not a <foo> object.

