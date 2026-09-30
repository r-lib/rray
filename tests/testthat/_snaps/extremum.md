# the output type of every pair of native types

    Code
      native_ptype_matrix(rray_pmax, c("x", "y"))
    Output
            y
      x      lgl       int       dbl      cpl chr raw list
        lgl  "logical" "integer" "double" NA  NA  NA  NA  
        int  "integer" "integer" "double" NA  NA  NA  NA  
        dbl  "double"  "double"  "double" NA  NA  NA  NA  
        cpl  NA        NA        NA       NA  NA  NA  NA  
        chr  NA        NA        NA       NA  NA  NA  NA  
        raw  NA        NA        NA       NA  NA  NA  NA  
        list NA        NA        NA       NA  NA  NA  NA  

---

    Code
      native_ptype_matrix(rray_pmin, c("x", "y"))
    Output
            y
      x      lgl       int       dbl      cpl chr raw list
        lgl  "logical" "integer" "double" NA  NA  NA  NA  
        int  "integer" "integer" "double" NA  NA  NA  NA  
        dbl  "double"  "double"  "double" NA  NA  NA  NA  
        cpl  NA        NA        NA       NA  NA  NA  NA  
        chr  NA        NA        NA       NA  NA  NA  NA  
        raw  NA        NA        NA       NA  NA  NA  NA  
        list NA        NA        NA       NA  NA  NA  NA  

# errors on incompatible dimensions

    Code
      rray_pmax(x, y)
    Condition
      Error in `rray_pmax()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 2.

---

    Code
      rray_pmin(x, y)
    Condition
      Error in `rray_pmin()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 2.

# errors on unsupported types

    Code
      rray_pmax(0+1i, 0+2i)
    Condition
      Error in `rray_pmax()`:
      ! Can't apply `max` to `x` <complex> and `y` <complex>.

---

    Code
      rray_pmin(0+1i, 0+2i)
    Condition
      Error in `rray_pmin()`:
      ! Can't apply `min` to `x` <complex> and `y` <complex>.

---

    Code
      rray_pmax("a", "b")
    Condition
      Error in `rray_pmax()`:
      ! Can't apply `max` to `x` <character> and `y` <character>.

---

    Code
      rray_pmin(as.raw(1), as.raw(2))
    Condition
      Error in `rray_pmin()`:
      ! Can't apply `min` to `x` <raw> and `y` <raw>.

---

    Code
      rray_pmax(list(1), list(2))
    Condition
      Error in `rray_pmax()`:
      ! Can't apply `max` to `x` <list> and `y` <list>.

# `na_rm` must be `TRUE` or `FALSE`

    Code
      rray_pmax(1L, 2L, na_rm = NA)
    Condition
      Error in `rray_pmax()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

---

    Code
      rray_pmin(1L, 2L, na_rm = 1)
    Condition
      Error in `rray_pmin()`:
      ! `na_rm` must be `TRUE` or `FALSE`.

# dots must be empty

    Code
      rray_pmax(1L, 2L, 3L)
    Condition
      Error in `rray_pmax()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 3L
      i Did you forget to name an argument?

---

    Code
      rray_pmin(1L, 2L, 3L)
    Condition
      Error in `rray_pmin()`:
      ! `...` must be empty.
      x Problematic argument:
      * ..1 = 3L
      i Did you forget to name an argument?

# errors on scalar and classed input

    Code
      rray_pmax(NULL, 1L)
    Condition
      Error in `rray_pmax()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_pmin(1L, NULL)
    Condition
      Error in `rray_pmin()`:
      ! `y` must be an array, not `NULL`.

---

    Code
      rray_pmax(x, 1L)
    Condition
      Error in `rray_pmax()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_pmin(1L, x)
    Condition
      Error in `rray_pmin()`:
      ! `y` must be a bare array, not a <foo> object.

