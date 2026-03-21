# can broadcast 0 to 0 but not 0 to N

    Code
      rray_broadcast(x, c(1L, 2L))
    Condition
      Error in `rray_broadcast()`:
      ! Can't broadcast dimension 1 from size 0 to 1.

# can't broadcast from N to M when N > 1 and N != M

    Code
      rray_broadcast(x, c(2L, 4L))
    Condition
      Error in `rray_broadcast()`:
      ! Can't broadcast dimension 2 from size 3 to 4.

# can't decrease dimensionality

    Code
      rray_broadcast(x, c(2L, 3L))
    Condition
      Error in `rray_broadcast()`:
      ! Can't broadcast from dimensionality 3 to 2. Can't decrease dimensionality.

# errors on non-array input

    Code
      rray_broadcast(NULL, 1L)
    Condition
      Error in `rray_broadcast()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_broadcast(mean, 1L)
    Condition
      Error in `rray_broadcast()`:
      ! `x` must be an array, not a function.

# errors on non-integer dimension_sizes

    Code
      rray_broadcast(1, 1)
    Condition
      Error in `rray_broadcast()`:
      ! `dimension_sizes` must be an integer vector, not the number 1.

---

    Code
      rray_broadcast(1, "a")
    Condition
      Error in `rray_broadcast()`:
      ! `dimension_sizes` must be an integer vector, not the string "a".

# errors on empty dimension_sizes

    Code
      rray_broadcast(1, integer())
    Condition
      Error in `rray_broadcast()`:
      ! `dimension_sizes` must have at least one element.

# errors on missing dimension_sizes

    Code
      rray_broadcast(1, NA_integer_)
    Condition
      Error in `rray_broadcast()`:
      ! `dimension_sizes` must not contain missing values.

# errors on negative dimension_sizes

    Code
      rray_broadcast(1, -1L)
    Condition
      Error in `rray_broadcast()`:
      ! `dimension_sizes` must not contain negative values.

