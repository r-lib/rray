# errors when capacity would change

    Code
      rray_reshape(1:6, c(6L, 2L))
    Condition
      Error in `rray_reshape()`:
      ! Can't reshape to these dimension sizes. Can't change from a capacity of 6 to a capacity of 12.

# errors on non-array input

    Code
      rray_reshape(NULL, 1L)
    Condition
      Error in `rray_reshape()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_reshape(mean, 1L)
    Condition
      Error in `rray_reshape()`:
      ! `x` must be an array, not a function.

# errors on non-integer dimension_sizes

    Code
      rray_reshape(1, 1)
    Condition
      Error in `rray_reshape()`:
      ! `dimension_sizes` must be an integer vector, not the number 1.

---

    Code
      rray_reshape(1, "a")
    Condition
      Error in `rray_reshape()`:
      ! `dimension_sizes` must be an integer vector, not the string "a".

# errors on empty dimension_sizes

    Code
      rray_reshape(1, integer())
    Condition
      Error in `rray_reshape()`:
      ! `dimension_sizes` must have at least one element.

# errors on missing dimension_sizes

    Code
      rray_reshape(1, NA_integer_)
    Condition
      Error in `rray_reshape()`:
      ! `dimension_sizes` must not contain missing values.

# errors on negative dimension_sizes

    Code
      rray_reshape(1, -1L)
    Condition
      Error in `rray_reshape()`:
      ! `dimension_sizes` must not contain negative values.

