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
      rray_ptype_common(1L, NULL)
    Condition
      Error in `rray_ptype_common()`:
      ! `..2` must be an array, not `NULL`.

# errors on classed input

    Code
      rray_ptype_common(1L, x)
    Condition
      Error in `rray_ptype_common()`:
      ! `..2` must be a bare array, not a <foo> object.

# `.arg` names `...` as a whole

    Code
      rray_ptype_common(1L, "a", .arg = "foo")
    Condition
      Error in `rray_ptype_common()`:
      ! Can't combine `foo[[1]]` <integer> and `foo[[2]]` <character>.

---

    Code
      rray_ptype_common(x = 1L, y = "a", .arg = "foo")
    Condition
      Error in `rray_ptype_common()`:
      ! Can't combine `foo$x` <integer> and `foo$y` <character>.

# errors on a bad `.ptype`

    Code
      rray_ptype_common(1L, .ptype = sum)
    Condition
      Error in `rray_ptype_common()`:
      ! `.ptype` must be an array, not a primitive function.

---

    Code
      rray_ptype_common(1L, .ptype = structure(1, class = "foo"))
    Condition
      Error in `rray_ptype_common()`:
      ! `.ptype` must be a bare array, not a <foo> object.

# `.ptype_arg` renames `.ptype` in the error

    Code
      rray_ptype_common(1L, .ptype = sum, .ptype_arg = "pt")
    Condition
      Error in `rray_ptype_common()`:
      ! `pt` must be an array, not a primitive function.

---

    Code
      rray_ptype_common(1L, .ptype = sum, .ptype_arg = "")
    Condition
      Error in `rray_ptype_common()`:
      ! Input must be an array, not a primitive function.

