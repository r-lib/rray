# returns logical output for every supported type pair

    Code
      native_ptype_matrix(rray_equal, c("x", "y"))
    Output
            y
      x      lgl       int       dbl       cpl       chr raw list
        lgl  "logical" "logical" "logical" "logical" NA  NA  NA  
        int  "logical" "logical" "logical" "logical" NA  NA  NA  
        dbl  "logical" "logical" "logical" "logical" NA  NA  NA  
        cpl  "logical" "logical" "logical" "logical" NA  NA  NA  
        chr  NA        NA        NA        NA        NA  NA  NA  
        raw  NA        NA        NA        NA        NA  NA  NA  
        list NA        NA        NA        NA        NA  NA  NA  

# errors on incompatible dimensions

    Code
      rray_equal(x, y)
    Condition
      Error in `rray_equal()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 2.

# errors on unsupported types

    Code
      rray_equal("a", "b")
    Condition
      Error in `rray_equal()`:
      ! Can't apply `==` to `x` <character> and `y` <character>.

---

    Code
      rray_not_equal(as.raw(1), as.raw(2))
    Condition
      Error in `rray_not_equal()`:
      ! Can't apply `!=` to `x` <raw> and `y` <raw>.

# a type error beats a dimension error

    Code
      rray_equal(x, y)
    Condition
      Error in `rray_equal()`:
      ! Can't apply `==` to `x` <character> and `y` <character>.

# errors on scalar and classed input

    Code
      rray_equal(NULL, 0+1i)
    Condition
      Error in `rray_equal()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_not_equal(0+1i, NULL)
    Condition
      Error in `rray_not_equal()`:
      ! `y` must be an array, not `NULL`.

---

    Code
      rray_equal(x, 0+1i)
    Condition
      Error in `rray_equal()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_not_equal(0+1i, x)
    Condition
      Error in `rray_not_equal()`:
      ! `y` must be a bare array, not a <foo> object.

