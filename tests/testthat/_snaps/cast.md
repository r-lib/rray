# which pairs of native types convert

    Code
      native_ptype_matrix(rray_cast, c("from", "to"))
    Output
            to
      from   lgl       int       dbl      cpl       chr         raw   list  
        lgl  "logical" "integer" "double" "complex" NA          NA    NA    
        int  "logical" "integer" "double" "complex" NA          NA    NA    
        dbl  "logical" "integer" "double" "complex" NA          NA    NA    
        cpl  NA        NA        NA       "complex" NA          NA    NA    
        chr  NA        NA        NA       NA        "character" NA    NA    
        raw  NA        NA        NA       NA        NA          "raw" NA    
        list NA        NA        NA       NA        NA          NA    "list"

# errors on a lossy cast, reporting the location

    Code
      rray_cast(c(1, 2.5), integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <double> to <integer> due to loss of precision at location 2.

---

    Code
      rray_cast(c(0, 1, 2), logical())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <double> to <logical> due to loss of precision at location 3.

---

    Code
      rray_cast(c(1L, 5L), logical())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <integer> to <logical> due to loss of precision at location 2.

# errors when a double is out of integer range

    Code
      rray_cast(2^31, integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <double> to <integer> due to loss of precision at location 1.

---

    Code
      rray_cast(-2^31, integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <double> to <integer> due to loss of precision at location 1.

# errors on types that don't convert

    Code
      rray_cast(letters, integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <character> to <integer>.

---

    Code
      rray_cast(as.raw(1), integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <raw> to <integer>.

---

    Code
      rray_cast(list(1), double())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <list> to <double>.

---

    Code
      rray_cast(1L, character())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <integer> to <character>.

# complex is a one way trip

    Code
      rray_cast(0+1i, double())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <complex> to <double>.

---

    Code
      rray_cast(0+1i, integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <complex> to <integer>.

---

    Code
      rray_cast(0+1i, logical())
    Condition
      Error in `rray_cast()`:
      ! Can't convert `x` from <complex> to <logical>.

# the error names the failing element of `...`

    Code
      rray_cast_common(1L, 2.5, .to = integer())
    Condition
      Error in `rray_cast_common()`:
      ! Can't convert `..2` from <double> to <integer> due to loss of precision at location 1.

# errors on non-array input

    Code
      rray_cast(NULL, integer())
    Condition
      Error in `rray_cast()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_cast(1L, NULL)
    Condition
      Error in `rray_cast()`:
      ! `to` must be an array, not `NULL`.

# errors on classed input

    Code
      rray_cast(x, integer())
    Condition
      Error in `rray_cast()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_cast(1, x)
    Condition
      Error in `rray_cast()`:
      ! `to` must be a bare array, not a <foo> object.

---

    Code
      rray_cast_common(1, .to = x)
    Condition
      Error in `rray_cast_common()`:
      ! `.to` must be a bare array, not a <foo> object.

