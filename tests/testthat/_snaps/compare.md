# returns logical output for every supported type pair

    Code
      native_ptype_matrix(rray_equal, c("x", "y"))
    Output
            y
      x      lgl       int       dbl       cpl chr raw list
        lgl  "logical" "logical" "logical" NA  NA  NA  NA  
        int  "logical" "logical" "logical" NA  NA  NA  NA  
        dbl  "logical" "logical" "logical" NA  NA  NA  NA  
        cpl  NA        NA        NA        NA  NA  NA  NA  
        chr  NA        NA        NA        NA  NA  NA  NA  
        raw  NA        NA        NA        NA  NA  NA  NA  
        list NA        NA        NA        NA  NA  NA  NA  

# errors on incompatible dimensions

    Code
      rray_equal(x, y)
    Condition
      Error in `rray_equal()`:
      ! Can't find common dimensions at axis 1. `x` has dimension 3 and `y` has dimension 2.

# errors on unsupported types

    Code
      rray_equal(0+1i, 0+1i)
    Condition
      Error in `rray_equal()`:
      ! Can't apply `==` to `x` <complex> and `y` <complex>.

---

    Code
      rray_not_equal("a", "b")
    Condition
      Error in `rray_not_equal()`:
      ! Can't apply `!=` to `x` <character> and `y` <character>.

---

    Code
      rray_greater_than(as.raw(1), as.raw(2))
    Condition
      Error in `rray_greater_than()`:
      ! Can't apply `>` to `x` <raw> and `y` <raw>.

---

    Code
      rray_greater_than_or_equal(list(1), list(1))
    Condition
      Error in `rray_greater_than_or_equal()`:
      ! Can't apply `>=` to `x` <list> and `y` <list>.

---

    Code
      rray_less_than(0+1i, 0+1i)
    Condition
      Error in `rray_less_than()`:
      ! Can't apply `<` to `x` <complex> and `y` <complex>.

---

    Code
      rray_less_than_or_equal("a", "b")
    Condition
      Error in `rray_less_than_or_equal()`:
      ! Can't apply `<=` to `x` <character> and `y` <character>.

# a type error beats a dimension error

    Code
      rray_equal(x, y)
    Condition
      Error in `rray_equal()`:
      ! Can't apply `==` to `x` <character> and `y` <character>.

# errors on scalar and classed input

    Code
      rray_equal(NULL, 1L)
    Condition
      Error in `rray_equal()`:
      ! `x` must be an array, not `NULL`.

---

    Code
      rray_not_equal(1L, NULL)
    Condition
      Error in `rray_not_equal()`:
      ! `y` must be an array, not `NULL`.

---

    Code
      rray_greater_than(x, 1L)
    Condition
      Error in `rray_greater_than()`:
      ! `x` must be a bare array, not a <foo> object.

---

    Code
      rray_less_than(1L, x)
    Condition
      Error in `rray_less_than()`:
      ! `y` must be a bare array, not a <foo> object.

