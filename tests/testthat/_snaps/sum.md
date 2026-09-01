# errors on axes out of range

    Code
      rray_sum(x, 3L)
    Condition
      Error in `rray_sum()`:
      ! `axes` must contain values less than or equal to the dimensionality of 2, not 3.

# errors on axes not in strictly increasing order

    Code
      rray_sum(x, c(1L, 1L))
    Condition
      Error in `rray_sum()`:
      ! `axes` must be in strictly increasing order.

---

    Code
      rray_sum(x, c(2L, 1L))
    Condition
      Error in `rray_sum()`:
      ! `axes` must be in strictly increasing order.

# errors on axes less than 1

    Code
      rray_sum(x, 0L)
    Condition
      Error in `rray_sum()`:
      ! `axes` must contain values greater than or equal to 1, not 0.

# errors on axes with NA

    Code
      rray_sum(x, NA_integer_)
    Condition
      Error in `rray_sum()`:
      ! `axes` must not contain missing values.

# errors on integer overflow

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow in `rray_sum()`.

# errors on integer underflow

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! Integer overflow in `rray_sum()`.

# errors on non-numeric input

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! `x` must be a logical, integer, double, or complex array, not a character matrix.

---

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! `x` must be a logical, integer, double, or complex array, not a raw matrix.

# errors on classed input

    Code
      rray_sum(x, 1L)
    Condition
      Error in `rray_sum()`:
      ! `x` must be a bare array, not a <foo> object.

