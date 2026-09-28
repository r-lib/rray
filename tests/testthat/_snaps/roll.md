# `n` and `axes` must be named

    Code
      rray_roll(x, 1, 2)
    Condition
      Error in `rray_roll()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 1
      * ..2 = 2
      i Did you forget to name an argument?
    Code
      rray_roll(x, 1, axes = 2)
    Condition
      Error in `rray_roll()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# `axes` is validated

    Code
      rray_roll(x, n = 1, axes = c(1, 1))
    Condition
      Error in `rray_roll()`:
      ! `axes` must be in strictly increasing order.

---

    Code
      rray_roll(x, n = 1, axes = c(2, 1))
    Condition
      Error in `rray_roll()`:
      ! `axes` must be in strictly increasing order.

---

    Code
      rray_roll(x, n = 1, axes = 3)
    Condition
      Error in `rray_roll()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_roll(x, n = 1, axes = 0)
    Condition
      Error in `rray_roll()`:
      ! `axes` must contain values greater than or equal to 1, not 0.

---

    Code
      rray_roll(x, n = 1, axes = NA)
    Condition
      Error in `rray_roll()`:
      ! `axes` must not contain missing values.

# `n` is validated

    Code
      rray_roll(x, n = c(1, 2, 3), axes = c(1, 2))
    Condition
      Error in `rray_roll()`:
      ! `n` must be size 1 or size 2 to match `axes`, not size 3.

---

    Code
      rray_roll(x, n = c(1, 2), axes = 1)
    Condition
      Error in `rray_roll()`:
      ! `n` must be size 1, not size 2.

---

    Code
      rray_roll(x, n = integer(), axes = 1)
    Condition
      Error in `rray_roll()`:
      ! `n` must be size 1, not size 0.

---

    Code
      rray_roll(x, n = NA, axes = 1)
    Condition
      Error in `rray_roll()`:
      ! `n` must not contain missing values.

---

    Code
      rray_roll(x, n = c(1L, NA), axes = c(1, 2))
    Condition
      Error in `rray_roll()`:
      ! `n` must not contain missing values.

---

    Code
      rray_roll(x, n = 1.5, axes = 1)
    Condition
      Error:
      ! Can't convert from `n` <double> to <integer> due to loss of precision.
      * Locations: 1

---

    Code
      rray_roll(x, n = "a", axes = 1)
    Condition
      Error:
      ! Can't convert `n` <character> to <integer>.

---

    Code
      rray_roll(x, n = c(a = 1L), axes = 1)
    Condition
      Error in `rray_roll()`:
      ! `n` can't have attributes.

---

    Code
      rray_roll(x, n = matrix(1L), axes = 1)
    Condition
      Error in `rray_roll()`:
      ! `n` can't have attributes.

# errors on invalid input

    Code
      rray_roll(NULL, n = 1, axes = 1)
    Condition
      Error in `rray_roll()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_roll(x, n = 1, axes = 1)
    Condition
      Error in `rray_roll()`:
      ! `x` must be a bare array, not a <foo> object.

# `n` and `axis` must be named

    Code
      rray_roll_each(x, 1, 2)
    Condition
      Error in `rray_roll_each()`:
      ! `...` must be empty.
      x Problematic arguments:
      * ..1 = 1
      * ..2 = 2
      i Did you forget to name an argument?
    Code
      rray_roll_each(x, 1, axis = 2)
    Condition
      Error in `rray_roll_each()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 1
      i Did you forget to name an argument?

# `axis` is validated

    Code
      rray_roll_each(x, n = 1, axis = c(1, 2))
    Condition
      Error in `rray_roll_each()`:
      ! `axis` must be a single integer, not length 2.

---

    Code
      rray_roll_each(x, n = 1, axis = 3)
    Condition
      Error in `rray_roll_each()`:
      ! `axis` must be less than or equal to the dimensionality of 2, not 3.

---

    Code
      rray_roll_each(x, n = 1, axis = 0)
    Condition
      Error in `rray_roll_each()`:
      ! `axis` must be greater than or equal to 1, not 0.

---

    Code
      rray_roll_each(x, n = 1, axis = NA_integer_)
    Condition
      Error in `rray_roll_each()`:
      ! `axis` must not be missing.

# `n` must broadcast to `x` with `axis` set to 1

    Code
      rray_roll_each(x, n = c(0, 1, 2, 3), axis = 1)
    Condition
      Error in `rray_roll_each()`:
      ! Can't broadcast axis 1 of `n` from dimension 4 to 1.

---

    Code
      rray_roll_each(x, n = c(1, 2), axis = 2)
    Condition
      Error in `rray_roll_each()`:
      ! Can't broadcast axis 1 of `n` from dimension 2 to 3.

---

    Code
      rray_roll_each(x, n = array(1L, c(1, 1, 2)), axis = 1)
    Condition
      Error in `rray_roll_each()`:
      ! Can't broadcast `n` from dimensionality 3 to 2. Can't decrease dimensionality.

---

    Code
      rray_roll_each(x, n = c(1, 2, 3), axis = 2)
    Condition
      Error in `rray_roll_each()`:
      ! Can't broadcast axis 1 of `n` from dimension 3 to 2.

# `n` must be an integer array with no missing values

    Code
      rray_roll_each(x, n = NA, axis = 1)
    Condition
      Error in `rray_roll_each()`:
      ! `n` must not contain missing values.

---

    Code
      rray_roll_each(x, n = c(1L, NA, 3L), axis = 2)
    Condition
      Error in `rray_roll_each()`:
      ! `n` must not contain missing values.

---

    Code
      rray_roll_each(x, n = 1.5, axis = 1)
    Condition
      Error in `rray_roll_each()`:
      ! Can't convert from `n` <double> to <integer> due to loss of precision at location 1.

---

    Code
      rray_roll_each(x, n = "a", axis = 1)
    Condition
      Error in `rray_roll_each()`:
      ! Can't convert from `n` <character> to <integer>.

---

    Code
      rray_roll_each(x, n = factor("a"), axis = 1)
    Condition
      Error in `rray_roll_each()`:
      ! `n` must be a bare array, not a <factor> object.

---

    Code
      rray_roll_each(x, n = NULL, axis = 1)
    Condition
      Error in `rray_roll_each()`:
      ! `n` must be an array, not `NULL`.

# `x` must be a bare array

    Code
      rray_roll_each(NULL, n = 1, axis = 1)
    Condition
      Error in `rray_roll_each()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_roll_each(x, n = 1, axis = 1)
    Condition
      Error in `rray_roll_each()`:
      ! `x` must be a bare array, not a <foo> object.

