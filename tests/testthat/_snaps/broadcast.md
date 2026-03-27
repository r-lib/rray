# can broadcast 0 to 0 but not 0 to N

    Code
      rray_broadcast(x, c(1L, 2L))
    Condition
      Error in `rray_broadcast()`:
      ! Can't broadcast axis 1 from dimension 0 to 1.

# can't broadcast from N to M when N > 1 and N != M

    Code
      rray_broadcast(x, c(2L, 4L))
    Condition
      Error in `rray_broadcast()`:
      ! Can't broadcast axis 2 from dimension 3 to 4.

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

# coerces dimensions to integer

    Code
      rray_broadcast(1, 2.5)
    Condition
      Error:
      ! Can't convert from `dimensions` <double> to <integer> due to loss of precision.
      * Locations: 1

# errors on non-coercible dimensions

    Code
      rray_broadcast(1, "a")
    Condition
      Error:
      ! Can't convert `dimensions` <character> to <integer>.

# errors on empty dimensions

    Code
      rray_broadcast(1, integer())
    Condition
      Error in `rray_broadcast()`:
      ! `dimensions` must have at least one element.

# errors on missing dimensions

    Code
      rray_broadcast(1, NA_integer_)
    Condition
      Error in `rray_broadcast()`:
      ! `dimensions` must not contain missing values.

# errors on negative dimensions

    Code
      rray_broadcast(1, -1L)
    Condition
      Error in `rray_broadcast()`:
      ! `dimensions` must not contain negative values.

# errors on dimensionality upper bound

    Code
      rray_broadcast(array(1, dim = rep(1L, 64)), rep(1L, 65))
    Condition
      Error in `rray_broadcast()`:
      ! rray can't support arrays with a dimensionality greater than 64. A dimensionality of 65 was requested.

