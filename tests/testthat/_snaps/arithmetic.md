# errors on incompatible dimensions

    Code
      rray_add(x, array(1L, c(2L, 2L)))
    Condition
      Error in `rray_add()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 2.

---

    Code
      rray_add(x, array(integer(), c(0L, 2L)))
    Condition
      Error in `rray_add()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 0.

# errors on incompatible types

    Code
      rray_add(1L, "a")
    Condition
      Error in `rray_add()`:
      ! Can't combine `x` <integer> and `y` <character>.

---

    Code
      rray_add(as.raw(1), 1L)
    Condition
      Error in `rray_add()`:
      ! Can't combine `x` <raw> and `y` <integer>.

# errors on types `+` doesn't support

    Code
      rray_add("a", "b")
    Condition
      Error in `rray_add()`:
      ! Can't apply `+` to `x` <character> and `y` <character>.

---

    Code
      rray_add(as.raw(1), as.raw(1))
    Condition
      Error in `rray_add()`:
      ! Can't apply `+` to `x` <raw> and `y` <raw>.

---

    Code
      rray_add(list(1), list(2))
    Condition
      Error in `rray_add()`:
      ! Can't apply `+` to `x` <list> and `y` <list>.

# errors on integer overflow

    Code
      rray_add(.Machine$integer.max, 1L)
    Condition
      Error in `rray_add()`:
      ! Integer overflow.

# errors on integer underflow

    Code
      rray_add(-.Machine$integer.max, -1L)
    Condition
      Error in `rray_add()`:
      ! Integer overflow.

# errors on scalar input

    Code
      rray_add(NULL, 1L)
    Condition
      Error in `rray_add()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_add(1L, NULL)
    Condition
      Error in `rray_add()`:
      ! `y` must be an array, not `NULL`.

# errors on classed input

    Code
      rray_add(x, 1L)
    Condition
      Error in `rray_add()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_add(1L, x)
    Condition
      Error in `rray_add()`:
      ! `y` must be a bare array, not a <foo> object.

