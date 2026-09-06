# the output type of every pair of native types

    Code
      native_ptype_matrix(rray_subtract, c("x", "y"))
    Output
            y
      x      lgl       int       dbl       cpl       chr raw list
        lgl  "integer" "integer" "double"  "complex" NA  NA  NA  
        int  "integer" "integer" "double"  "complex" NA  NA  NA  
        dbl  "double"  "double"  "double"  "complex" NA  NA  NA  
        cpl  "complex" "complex" "complex" "complex" NA  NA  NA  
        chr  NA        NA        NA        NA        NA  NA  NA  
        raw  NA        NA        NA        NA        NA  NA  NA  
        list NA        NA        NA        NA        NA  NA  NA  

# errors on incompatible dimensions

    Code
      rray_subtract(x, array(1L, c(2L, 2L)))
    Condition
      Error in `rray_subtract()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 2.

---

    Code
      rray_subtract(x, array(integer(), c(0L, 2L)))
    Condition
      Error in `rray_subtract()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 0.

# errors on types `-` doesn't support

    Code
      rray_subtract("a", "b")
    Condition
      Error in `rray_subtract()`:
      ! Can't apply `-` to `x` <character> and `y` <character>.

---

    Code
      rray_subtract(as.raw(1), as.raw(1))
    Condition
      Error in `rray_subtract()`:
      ! Can't apply `-` to `x` <raw> and `y` <raw>.

---

    Code
      rray_subtract(list(1), list(2))
    Condition
      Error in `rray_subtract()`:
      ! Can't apply `-` to `x` <list> and `y` <list>.

---

    Code
      rray_subtract(1L, "a")
    Condition
      Error in `rray_subtract()`:
      ! Can't apply `-` to `x` <integer> and `y` <character>.

---

    Code
      rray_subtract(as.raw(1), 1L)
    Condition
      Error in `rray_subtract()`:
      ! Can't apply `-` to `x` <raw> and `y` <integer>.

# a type error beats a dimension error

    Code
      rray_subtract(x, y)
    Condition
      Error in `rray_subtract()`:
      ! Can't apply `-` to `x` <character> and `y` <character>.

# errors on integer overflow

    Code
      rray_subtract(.Machine$integer.max, -1L)
    Condition
      Error in `rray_subtract()`:
      ! Integer overflow.

# errors on integer underflow

    Code
      rray_subtract(-.Machine$integer.max, 1L)
    Condition
      Error in `rray_subtract()`:
      ! Integer overflow.

# errors on scalar input

    Code
      rray_subtract(NULL, 1L)
    Condition
      Error in `rray_subtract()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_subtract(1L, NULL)
    Condition
      Error in `rray_subtract()`:
      ! `y` must be an array, not `NULL`.

# errors on classed input

    Code
      rray_subtract(x, 1L)
    Condition
      Error in `rray_subtract()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_subtract(1L, x)
    Condition
      Error in `rray_subtract()`:
      ! `y` must be a bare array, not a <foo> object.

