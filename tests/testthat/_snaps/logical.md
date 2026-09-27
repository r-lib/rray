# returns logical output for logical input only

    Code
      native_ptype_matrix(rray_and, c("x", "y"))
    Output
            y
      x      lgl       int dbl cpl chr raw list
        lgl  "logical" NA  NA  NA  NA  NA  NA  
        int  NA        NA  NA  NA  NA  NA  NA  
        dbl  NA        NA  NA  NA  NA  NA  NA  
        cpl  NA        NA  NA  NA  NA  NA  NA  
        chr  NA        NA  NA  NA  NA  NA  NA  
        raw  NA        NA  NA  NA  NA  NA  NA  
        list NA        NA  NA  NA  NA  NA  NA  

# errors on incompatible dimensions

    Code
      rray_and(x, y)
    Condition
      Error in `rray_and()`:
      ! Can't find common dimensions at axis 2. `x` has dimension 2 and `y` has dimension 0.

# errors on non-logical input

    Code
      rray_and(1L, TRUE)
    Condition
      Error in `rray_and()`:
      ! `x` must be a logical array, not an integer 1D array.

---

    Code
      rray_or(TRUE, 1)
    Condition
      Error in `rray_or()`:
      ! `y` must be a logical array, not a double 1D array.

---

    Code
      rray_xor(array("a", c(2L, 2L)), TRUE)
    Condition
      Error in `rray_xor()`:
      ! `x` must be a logical array, not a character matrix.

# checks `x` before `y`

    Code
      rray_and(1L, 1)
    Condition
      Error in `rray_and()`:
      ! `x` must be a logical array, not an integer 1D array.

# a type error beats a dimension error

    Code
      rray_and(x, y)
    Condition
      Error in `rray_and()`:
      ! `x` must be a logical array, not an integer matrix.

# errors on scalar and classed input

    Code
      rray_and(NULL, TRUE)
    Condition
      Error in `rray_and()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_or(TRUE, NULL)
    Condition
      Error in `rray_or()`:
      ! `y` must be an array, not `NULL`.

---

    Code
      rray_xor(x, TRUE)
    Condition
      Error in `rray_xor()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_and(TRUE, x)
    Condition
      Error in `rray_and()`:
      ! `y` must be a bare array, not a <foo> object.

