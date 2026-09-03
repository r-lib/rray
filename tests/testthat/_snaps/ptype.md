# errors on types that don't combine

    Code
      rray_ptype2(character(), integer())
    Condition
      Error in `rray_ptype2()`:
      ! Can't combine `x` <character> and `y` <integer>.

---

    Code
      rray_ptype2(raw(), integer())
    Condition
      Error in `rray_ptype2()`:
      ! Can't combine `x` <raw> and `y` <integer>.

---

    Code
      rray_ptype2(list(), double())
    Condition
      Error in `rray_ptype2()`:
      ! Can't combine `x` <list> and `y` <double>.

---

    Code
      rray_ptype2(character(), list())
    Condition
      Error in `rray_ptype2()`:
      ! Can't combine `x` <character> and `y` <list>.

# the error names the input that set the common type

    Code
      rray_ptype_common(1L, "a")
    Condition
      Error in `rray_ptype_common()`:
      ! Can't combine `..1` <integer> and `..2` <character>.

---

    Code
      rray_ptype_common(1L, 2.5, "a")
    Condition
      Error in `rray_ptype_common()`:
      ! Can't combine `..2` <double> and `..3` <character>.

---

    Code
      rray_ptype_common(x = 1L, y = 2.5, z = "a")
    Condition
      Error in `rray_ptype_common()`:
      ! Can't combine `y` <double> and `z` <character>.

# errors on no inputs

    Code
      rray_ptype_common()
    Condition
      Error in `rray_ptype_common()`:
      ! Must supply at least one array to `...`.

# errors on non-array input

    Code
      rray_ptype2(NULL, integer())
    Condition
      Error in `rray_ptype2()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_ptype2(integer(), sum)
    Condition
      Error in `rray_ptype2()`:
      ! `y` must be an array, not a primitive function.

---

    Code
      rray_ptype_common(1L, NULL)
    Condition
      Error in `rray_ptype_common()`:
      ! `..2` must be an array, not `NULL`.

# errors on classed input

    Code
      rray_ptype2(x, integer())
    Condition
      Error in `rray_ptype2()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_ptype2(integer(), x)
    Condition
      Error in `rray_ptype2()`:
      ! `y` must be a bare array, not a <foo> object.

---

    Code
      rray_ptype_common(1L, x)
    Condition
      Error in `rray_ptype_common()`:
      ! `..2` must be a bare array, not a <foo> object.

