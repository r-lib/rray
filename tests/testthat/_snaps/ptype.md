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

