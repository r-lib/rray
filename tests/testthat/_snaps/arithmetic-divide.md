# the output type of every pair of native types

    Code
      native_ptype_matrix(rray_divide, c("x", "y"))
    Output
            y
      x      lgl       int       dbl       cpl       chr raw list
        lgl  "double"  "double"  "double"  "complex" NA  NA  NA  
        int  "double"  "double"  "double"  "complex" NA  NA  NA  
        dbl  "double"  "double"  "double"  "complex" NA  NA  NA  
        cpl  "complex" "complex" "complex" "complex" NA  NA  NA  
        chr  NA        NA        NA        NA        NA  NA  NA  
        raw  NA        NA        NA        NA        NA  NA  NA  
        list NA        NA        NA        NA        NA  NA  NA  

# errors on incompatible dimensions

    Code
      rray_divide(x, array(1L, c(2L, 2L)))
    Condition
      Error in `rray_divide()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 2.

---

    Code
      rray_divide(x, array(integer(), c(0L, 2L)))
    Condition
      Error in `rray_divide()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 0.

# errors on types `/` doesn't support

    Code
      rray_divide("a", "b")
    Condition
      Error in `rray_divide()`:
      ! Can't apply `/` to `x` <character> and `y` <character>.

---

    Code
      rray_divide(as.raw(1), as.raw(1))
    Condition
      Error in `rray_divide()`:
      ! Can't apply `/` to `x` <raw> and `y` <raw>.

---

    Code
      rray_divide(list(1), list(2))
    Condition
      Error in `rray_divide()`:
      ! Can't apply `/` to `x` <list> and `y` <list>.

---

    Code
      rray_divide(1L, "a")
    Condition
      Error in `rray_divide()`:
      ! Can't apply `/` to `x` <integer> and `y` <character>.

---

    Code
      rray_divide(as.raw(1), 1L)
    Condition
      Error in `rray_divide()`:
      ! Can't apply `/` to `x` <raw> and `y` <integer>.

# a type error beats a dimension error

    Code
      rray_divide(x, y)
    Condition
      Error in `rray_divide()`:
      ! Can't apply `/` to `x` <character> and `y` <character>.

# errors on scalar input

    Code
      rray_divide(NULL, 1L)
    Condition
      Error in `rray_divide()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_divide(1L, NULL)
    Condition
      Error in `rray_divide()`:
      ! `y` must be an array, not `NULL`.

# errors on classed input

    Code
      rray_divide(x, 1L)
    Condition
      Error in `rray_divide()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_divide(1L, x)
    Condition
      Error in `rray_divide()`:
      ! `y` must be a bare array, not a <foo> object.

