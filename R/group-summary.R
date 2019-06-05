# Summary generics are:
# - all(), any()
# - sum(), prod()
# - min(), max()
# - range()

# All of them dispatch on the first argument, as described in `?Summary`

# Also included is xtfrm()

# ------------------------------------------------------------------------------

# xtfrm.vctrs_vctr() calls vec_proxy_compare(), which (as of 2019-06-04)
# converts arrays to data frames, varying the first axis fastest. This makes
# sense for the `vec_order()` functions, but results in bad behavior for
# arrays because we want a global xtfrm, not a rowwise one. Because
# min.vctrs_vctr uses xtfrm, this behavior leaks into these functions as well.

# So, for example, it will return 2 to indicate that the second row is the
# "min" row and then min.vctrs_vctr would just return the first value in the
# second row. This is definitely not the right behavior, as we want the
# global minimum of the array

# We do borrow from vctrs a bit and ignore the `...`
# but otherwise we fallback to the base R method and then return an rray

#' @export
min.vctrs_rray <- function(x, ..., na.rm = FALSE) {
  vec_math_base("min", vec_data(x), na.rm = na.rm)
}

#' @export
max.vctrs_rray <- function(x, ..., na.rm = FALSE) {
  vec_math_base("max", vec_data(x), na.rm = na.rm)
}

#' @export
xtfrm.vctrs_rray <- function(x) {
  vec_data(x)
}

#' @export
xtfrm.vctrs_rray_lgl <- function(x) {
  vec_cast_inner(vec_data(x), integer())
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

rray_range_vctrs_wrapper <- function(x, na.rm) {
  vec_math_base("range", vec_data(x), na.rm = na.rm)
}

# ------------------------------------------------------------------------------

rray_prod_vctrs_wrapper <- function(x, na.rm) {
  vec_math_base("prod", vec_data(x), na.rm = na.rm)
}

rray_sum_vctrs_wrapper <- function(x, na.rm) {
  vec_math_base("sum", vec_data(x), na.rm = na.rm)
}
