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
      ! Can't convert from `c(1, 2.5)` <double> to <integer> due to loss of precision at location 2.

---

    Code
      rray_cast(c(0, 1, 2), logical())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `c(0, 1, 2)` <double> to <logical> due to loss of precision at location 3.

---

    Code
      rray_cast(c(1L, 5L), logical())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `c(1L, 5L)` <integer> to <logical> due to loss of precision at location 2.

# errors when a double is out of integer range

    Code
      rray_cast(2^31, integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `2^31` <double> to <integer> due to loss of precision at location 1.

---

    Code
      rray_cast(-2^31, integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `-2^31` <double> to <integer> due to loss of precision at location 1.

# errors on types that don't convert

    Code
      rray_cast(letters, integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `letters` <character> to <integer>.

---

    Code
      rray_cast(as.raw(1), integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `as.raw(1)` <raw> to <integer>.

---

    Code
      rray_cast(list(1), double())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `list(1)` <list> to <double>.

---

    Code
      rray_cast(1L, character())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `1L` <integer> to <character>.

# complex is a one way trip

    Code
      rray_cast(0+1i, double())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `0 + (0+1i)` <complex> to <double>.

---

    Code
      rray_cast(0+1i, integer())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `0 + (0+1i)` <complex> to <integer>.

---

    Code
      rray_cast(0+1i, logical())
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `0 + (0+1i)` <complex> to <logical>.

# `x_arg` defaults to the caller's expression

    Code
      f(1.5)
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `myinput` <double> to <integer> due to loss of precision at location 1.

# `x_arg` can be overridden or emptied

    Code
      rray_cast(1.5, integer(), x_arg = "vals")
    Condition
      Error in `rray_cast()`:
      ! Can't convert from `vals` <double> to <integer> due to loss of precision at location 1.

---

    Code
      rray_cast(1.5, integer(), x_arg = "")
    Condition
      Error in `rray_cast()`:
      ! Can't convert from <double> to <integer> due to loss of precision at location 1.

---

    Code
      rray_cast("a", integer(), x_arg = "")
    Condition
      Error in `rray_cast()`:
      ! Can't convert from <character> to <integer>.

# `to_arg` is empty by default, so `to` is reported as `Input`

    Code
      rray_cast(1, sum)
    Condition
      Error in `rray_cast()`:
      ! Input must be an array, not a primitive function.

---

    Code
      rray_cast(1, sum, to_arg = "myto")
    Condition
      Error in `rray_cast()`:
      ! `myto` must be an array, not a primitive function.

# `...` must be empty

    Code
      rray_cast(1, integer(), 5)
    Condition
      Error in `rray_cast()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 5
      i Did you forget to name an argument?

# errors on non-array input

    Code
      rray_cast(NULL, integer())
    Condition
      Error in `rray_cast()`:
      ! `NULL` must be an array, not `NULL`.

---

    Code
      rray_cast(1L, NULL)
    Condition
      Error in `rray_cast()`:
      ! Input must be an array, not `NULL`.

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
      ! Input must be a bare array, not a <foo> object.

