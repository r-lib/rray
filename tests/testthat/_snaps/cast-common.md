# errors on no inputs when `.to` is not supplied

    Code
      rray_cast_common()
    Condition
      Error in `rray_cast_common()`:
      ! Must supply at least one array to `...`.

# errors when `...` has no common type

    Code
      rray_cast_common(1L, "a")
    Condition
      Error in `rray_cast_common()`:
      ! Can't combine `..1` <integer> and `..2` <character>.

# the error names the failing element of `...`

    Code
      rray_cast_common(1L, 2.5, .to = integer())
    Condition
      Error in `rray_cast_common()`:
      ! Can't convert `..2` from <double> to <integer> due to loss of precision at location 1.

# errors on a bad `.to`

    Code
      rray_cast_common(1, .to = sum)
    Condition
      Error in `rray_cast_common()`:
      ! `.to` must be an array, not a primitive function.

---

    Code
      rray_cast_common(1, .to = structure(1, class = "foo"))
    Condition
      Error in `rray_cast_common()`:
      ! `.to` must be a bare array, not a <foo> object.

