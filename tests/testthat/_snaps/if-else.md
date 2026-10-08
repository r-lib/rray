# condition must be an unclassed logical array

    Code
      rray_if_else(1:2, 1L, 2L)
    Condition
      Error in `rray_if_else()`:
      ! `condition` must be a logical array, not an integer vector.

---

    Code
      rray_if_else(structure(TRUE, class = "foo"), 1L, 2L)
    Condition
      Error in `rray_if_else()`:
      ! `condition` must be a bare array, not a <foo> object.

# each branch must be an array

    Code
      rray_if_else(TRUE, NULL, 1L)
    Condition
      Error in `rray_if_else()`:
      ! `true` must be an array, not `NULL`.

---

    Code
      rray_if_else(TRUE, 1L, NULL)
    Condition
      Error in `rray_if_else()`:
      ! `false` must be an array, not `NULL`.

---

    Code
      rray_if_else(TRUE, 1L, 2L, missing = mean)
    Condition
      Error in `rray_if_else()`:
      ! `missing` must be an array, not a function.

# all branches must have compatible types

    Code
      rray_if_else(TRUE, 1L, "a")
    Condition
      Error in `rray_if_else()`:
      ! Can't combine `true` <integer> and `false` <character>.

---

    Code
      rray_if_else(TRUE, 1L, 2L, missing = "a")
    Condition
      Error in `rray_if_else()`:
      ! Can't combine `true` <integer> and `missing` <character>.

# all branches must broadcast even when unselected

    Code
      rray_if_else(condition, bad, 1L)
    Condition
      Error in `rray_if_else()`:
      ! Can't find common dimensions at axis 1. `condition` has dimension 2 and `true` has dimension 3.

---

    Code
      rray_if_else(condition, 1L, bad)
    Condition
      Error in `rray_if_else()`:
      ! Can't find common dimensions at axis 1. `condition` has dimension 2 and `false` has dimension 3.

---

    Code
      rray_if_else(condition, 1L, 2L, missing = bad)
    Condition
      Error in `rray_if_else()`:
      ! Can't find common dimensions at axis 1. `condition` has dimension 2 and `missing` has dimension 3.

# explicit dimensions must fit every input

    Code
      rray_if_else(condition, true, 2L, dimensions = rray_dimensions(condition))
    Condition
      Error in `rray_if_else()`:
      ! Can't broadcast axis 2 of `true` from dimension 3 to 1.

---

    Code
      rray_if_else(condition, 1L, true, dimensions = rray_dimensions(condition))
    Condition
      Error in `rray_if_else()`:
      ! Can't broadcast axis 2 of `false` from dimension 3 to 1.

---

    Code
      rray_if_else(condition, 1L, 2L, missing = true, dimensions = rray_dimensions(
        condition))
    Condition
      Error in `rray_if_else()`:
      ! Can't broadcast axis 2 of `missing` from dimension 3 to 1.

---

    Code
      rray_if_else(condition, 1L, 2L, dimensions = c(3L, 1L))
    Condition
      Error in `rray_if_else()`:
      ! Can't broadcast axis 1 of `condition` from dimension 2 to 3.

# dots must be empty

    Code
      rray_if_else(TRUE, 1L, 2L, 3L)
    Condition
      Error in `rray_if_else()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 3L
      i Did you forget to name an argument?
