# Summary generics are:
# - all(), any()
# - sum(), prod()
# - min(), max()
# - range()

# All of them dispatch on the first argument, as described in `?Summary`

# ------------------------------------------------------------------------------

# - vctrs:::min.vctrs_vctr() does the right thing because of `xtfrm.vctrs_vctr()`
# - vctrs:::max.vctrs_vctr() does the right thing because of `xtfrm.vctrs_vctr()`

# However, vctrs only automatically implements `xtfrm()` for integer and double
# arrays, so logical ones need special treatment. Because of this, just
# implement a simple xtfrm() method that calls `vec_proxy_compare()` like vctrs

#' @export
xtfrm.vctrs_rray <- function(x) {
  vec_proxy_compare(x)
}

#' @export
xtfrm.vctrs_rray_lgl <- function(x) {
  rray_cast_inner(vec_proxy_compare(x), integer())
}

# ------------------------------------------------------------------------------

# This is a base R compatible version of `all()` and `any()`.
# It is used in vec_math() dispatch

# Note that `vctrs:::Summary.vctrs_vctr()` is how this is passed through,
# and `na.rm = TRUE` no matter what there!

rray_all_vctrs_wrapper <- function(x, na.rm) {
  vec_math_base("all", vec_data(x), na.rm = na.rm)
}

rray_any_vctrs_wrapper <- function(x, na.rm) {
  vec_math_base("any", vec_data(x), na.rm = na.rm)
}

# ------------------------------------------------------------------------------
