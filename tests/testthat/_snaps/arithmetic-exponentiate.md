# the output type of every pair of native types

    Code
      native_ptype_matrix(rray_exponentiate, c("x", "y"))
    Output
            y
      x      lgl      int      dbl      cpl chr raw list
        lgl  "double" "double" "double" NA  NA  NA  NA  
        int  "double" "double" "double" NA  NA  NA  NA  
        dbl  "double" "double" "double" NA  NA  NA  NA  
        cpl  NA       NA       NA       NA  NA  NA  NA  
        chr  NA       NA       NA       NA  NA  NA  NA  
        raw  NA       NA       NA       NA  NA  NA  NA  
        list NA       NA       NA       NA  NA  NA  NA  

# errors on incompatible dimensions

    Code
      rray_exponentiate(x, array(1L, c(2L, 2L)))
    Condition
      Error in `rray_exponentiate()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 2.

---

    Code
      rray_exponentiate(x, array(integer(), c(0L, 2L)))
    Condition
      Error in `rray_exponentiate()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 0.

# errors on types `^` doesn't support

    Code
      rray_exponentiate("a", "b")
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <character> and `y` <character>.

---

    Code
      rray_exponentiate(as.raw(1), as.raw(1))
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <raw> and `y` <raw>.

---

    Code
      rray_exponentiate(list(1), list(2))
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <list> and `y` <list>.

---

    Code
      rray_exponentiate(1L, "a")
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <integer> and `y` <character>.

---

    Code
      rray_exponentiate(as.raw(1), 1L)
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <raw> and `y` <integer>.

# errors on complex input

    Code
      rray_exponentiate(0+1i, 0+1i)
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <complex> and `y` <complex>.

---

    Code
      rray_exponentiate(1L, 0+1i)
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <integer> and `y` <complex>.

---

    Code
      rray_exponentiate(0+1i, 1L)
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <complex> and `y` <integer>.

---

    Code
      rray_exponentiate(1, 0+1i)
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <double> and `y` <complex>.

---

    Code
      rray_exponentiate(TRUE, 0+1i)
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <logical> and `y` <complex>.

# a type error beats a dimension error

    Code
      rray_exponentiate(x, y)
    Condition
      Error in `rray_exponentiate()`:
      ! Can't apply `^` to `x` <character> and `y` <character>.

# errors on scalar input

    Code
      rray_exponentiate(NULL, 1L)
    Condition
      Error in `rray_exponentiate()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_exponentiate(1L, NULL)
    Condition
      Error in `rray_exponentiate()`:
      ! `y` must be an array, not `NULL`.

# errors on classed input

    Code
      rray_exponentiate(x, 1L)
    Condition
      Error in `rray_exponentiate()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_exponentiate(1L, x)
    Condition
      Error in `rray_exponentiate()`:
      ! `y` must be a bare array, not a <foo> object.

