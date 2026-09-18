# `axis` is validated

    Code
      rray_split(x)
    Condition
      Error in `rray_split()`:
      ! argument "axis" is missing, with no default

---

    Code
      rray_split(x, 0, 1)
    Condition
      Error in `rray_split()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_split(x, 3, 1)
    Condition
      Error in `rray_split()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_split(x, c(1, 2), 1)
    Condition
      Error in `rray_split()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_split(x, NA_integer_, 1)
    Condition
      Error in `rray_split()`:
      ! `axis` must not be missing.

---

    Code
      rray_split(x, 1.5, 1)
    Condition
      Error:
      ! Can't convert from `axis` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      rray_split(x, structure(1L, class = "foo"), 1)
    Condition
      Error in `rray_split()`:
      ! `axis` can't have attributes.

# `dimensions` are validated

    Code
      rray_split(x, 1)
    Condition
      Error in `rray_split()`:
      ! argument "dimensions" is missing, with no default

---

    Code
      rray_split(x, 1, NA_integer_)
    Condition
      Error in `rray_split()`:
      ! `dimensions` must not contain missing values.

---

    Code
      rray_split(x, 1, 1.5)
    Condition
      Error:
      ! Can't convert from `dimensions` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      rray_split(x, 1, structure(1L, names = "a"))
    Condition
      Error in `rray_split()`:
      ! `dimensions` can't have attributes.

---

    Code
      rray_split(x, 1, 0)
    Condition
      Error in `rray_split()`:
      ! A single `dimensions` value must be positive, not 0.

---

    Code
      rray_split(x, 1, -1)
    Condition
      Error in `rray_split()`:
      ! `dimensions` must not contain negative values.

---

    Code
      rray_split(x, 2, 2)
    Condition
      Error in `rray_split()`:
      ! A single `dimensions` value of 2 must evenly divide the `axis` dimension of 3.

---

    Code
      rray_split(x, 1, c(1, -1))
    Condition
      Error in `rray_split()`:
      ! `dimensions` must not contain negative values.

---

    Code
      rray_split(x, 1, c(1, 2))
    Condition
      Error in `rray_split()`:
      ! `dimensions` must sum to the `axis` dimension of 2, not 3.

---

    Code
      rray_split(x, 2, c(1, 1))
    Condition
      Error in `rray_split()`:
      ! `dimensions` must sum to the `axis` dimension of 3, not 2.

---

    Code
      rray_split(x, 1, integer())
    Condition
      Error in `rray_split()`:
      ! `dimensions` must sum to the `axis` dimension of 2, not 0.

# errors on invalid input

    Code
      rray_split(NULL, 1, 1)
    Condition
      Error in `rray_split()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_split(x, 1, 1)
    Condition
      Error in `rray_split()`:
      ! `x` must be a bare array, not a <foo> object.

