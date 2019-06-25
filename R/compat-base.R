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
  vec_summary_base("min", x, na.rm = na.rm)
}

#' @export
max.vctrs_rray <- function(x, ..., na.rm = FALSE) {
  vec_summary_base("max", x, na.rm = na.rm)
}

#' @export
range.vctrs_rray <- function(x, ..., na.rm = FALSE) {
  vec_summary_base("range", x, na.rm = na.rm)
}

# should keep the inner type and container type
# (but the shape may be different, i.e. the min of a 2D matrix
# is a 1D vector of length 1)
vec_summary_base <- function(.fn, .x, ...) {
  value <- vec_math_base(.fn, .x, ...)
  value <- vec_cast_inner(value, .x)
  vec_cast_container(value, .x)
}

# ------------------------------------------------------------------------------

#' @export
xtfrm.vctrs_rray <- function(x) {
  vec_data(x)
}

#' @export
xtfrm.vctrs_rray_lgl <- function(x) {
  vec_cast_inner(vec_data(x), integer())
}

# ------------------------------------------------------------------------------

#' @export
determinant.vctrs_rray <- function(x, logarithm = TRUE, ...) {
  determinant(as.matrix(x), logarithm = logarithm, ...)
}

# ------------------------------------------------------------------------------

#' @export
is.na.vctrs_rray <- function(x) {
  res <- is.na(vec_data(x))
  vec_cast_container(res, x)
}

# Currently, I am choosing to use the vctrs implementation of
# `is.na<-vctrs_vctr`. It assigns entire rows to `NA`, and
# only accepts logicals of either length 1, or as long
# as the number of rows in `x`.
# We test for this in `test-compat-base.R`

# ------------------------------------------------------------------------------

