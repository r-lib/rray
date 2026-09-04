# the common type of every pair of native types

    Code
      native_ptype_matrix(rray_ptype2, c("x", "y"))
    Output
            y
      x      lgl       int       dbl       cpl       chr         raw   list  
        lgl  "logical" "integer" "double"  "complex" NA          NA    NA    
        int  "integer" "integer" "double"  "complex" NA          NA    NA    
        dbl  "double"  "double"  "double"  "complex" NA          NA    NA    
        cpl  "complex" "complex" "complex" "complex" NA          NA    NA    
        chr  NA        NA        NA        NA        "character" NA    NA    
        raw  NA        NA        NA        NA        NA          "raw" NA    
        list NA        NA        NA        NA        NA          NA    "list"

# errors on types that don't combine

    Code
      rray_ptype2(character(), integer())
    Condition
      Error:
      ! Can't combine `character()` <character> and `integer()` <integer>.

---

    Code
      rray_ptype2(raw(), integer())
    Condition
      Error:
      ! Can't combine `raw()` <raw> and `integer()` <integer>.

---

    Code
      rray_ptype2(list(), double())
    Condition
      Error:
      ! Can't combine `list()` <list> and `double()` <double>.

---

    Code
      rray_ptype2(character(), list())
    Condition
      Error:
      ! Can't combine `character()` <character> and `list()` <list>.

# `x_arg` and `y_arg` default to the caller's expression

    Code
      rray_ptype2(1L, "a")
    Condition
      Error:
      ! Can't combine `1L` <integer> and `"a"` <character>.

---

    Code
      f(1L, "a")
    Condition
      Error in `f()`:
      ! Can't combine `lhs` <integer> and `rhs` <character>.

# `x_arg` and `y_arg` can be overridden

    Code
      rray_ptype2(1L, "a", x_arg = "lhs", y_arg = "rhs")
    Condition
      Error:
      ! Can't combine `lhs` <integer> and `rhs` <character>.

# an empty arg is left out of the message

    Code
      rray_ptype2(1L, "a", x_arg = "", y_arg = "")
    Condition
      Error:
      ! Can't combine <integer> and <character>.

# `call` blames the caller, and can be overridden

    Code
      f(1L, "a")
    Condition
      Error in `f()`:
      ! Can't combine `a` <integer> and `b` <character>.

---

    Code
      outer()
    Condition
      Error in `outer()`:
      ! Can't combine `a` <integer> and `b` <character>.

# `...` must be empty

    Code
      rray_ptype2(1L, 2L, 5)
    Condition
      Error in `rray_ptype2()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 5
      i Did you forget to name an argument?

# errors on non-array input

    Code
      rray_ptype2(NULL, integer())
    Condition
      Error:
      ! `NULL` must be an array, not `NULL`.

---

    Code
      rray_ptype2(integer(), sum)
    Condition
      Error:
      ! `sum` must be an array, not a primitive function.

# errors on classed input

    Code
      rray_ptype2(x, integer())
    Condition
      Error:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_ptype2(integer(), x)
    Condition
      Error:
      ! `x` must be a bare array, not a <foo> object.

