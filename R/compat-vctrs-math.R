#' @export
vec_math.vctrs_rray <- function(fun, x, ...) {
  f <- rray_math_unary_op_switch(fun)
  f(x, ...)
}

rray_math_unary_op_switch <- function(fun) {
  switch(
    fun,

    # basic
    "abs" = rray_abs,
    "sign" = rray_sign,

    # exponential
    "exp" = rray_exp,
    "expm1" = rray_expm1,
    "log" = rray_log_vctrs_wrapper,
    "log2" = rray_log2,
    "log10" = rray_log10,
    "log1p" = rray_log1p,

    # power
    "sqrt" = rray_sqrt,

    # trigonometric
    "sin" = rray_sin,
    "cos" = rray_cos,
    "tan" = rray_tan,
    "asin" = rray_asin,
    "acos" = rray_acos,
    "atan" = rray_atan,
    "sinpi" = rray_sinpi,
    "cospi" = rray_cospi,
    "tanpi" = rray_tanpi,

    # hyperbolic
    "sinh" = rray_sinh,
    "cosh" = rray_cosh,
    "tanh" = rray_tanh,
    "asinh" = rray_asinh,
    "acosh" = rray_acosh,
    "atanh" = rray_atanh,

    # error and gamma
    "gamma" = rray_gamma,
    "lgamma" = rray_lgamma,
    "digamma" = rray_digamma,
    "trigamma" = rray_trigamma,

    # rounding
    "ceiling" = rray_ceiling,
    "floor" = rray_floor,
    "trunc" = rray_trunc_vctrs_wrapper,
    "round" = rray_round,
    "signif" = rray_signif,

    # finite
    "is.nan" = rray_is_nan,
    "is.infinite" = rray_is_infinite,
    "is.finite" = rray_is_finite,

    # summary
    "all" = rray_all_vctrs_wrapper,
    "any" = rray_any_vctrs_wrapper,
    "range" = rray_range_vctrs_wrapper,
    "prod" = rray_prod_vctrs_wrapper,
    "sum" = rray_sum_vctrs_wrapper,

    # cumulative
    "cummax" = rray_cummax_vctrs_wrapper,
    "cummin" = rray_cummin_vctrs_wrapper,
    "cumsum" = rray_cumsum_vctrs_wrapper,
    "cumprod" = rray_cumprod_vctrs_wrapper,

    glubort("Unary math function not known: {fun}.")
  )
}

# ------------------------------------------------------------------------------

rray_log_vctrs_wrapper <- function(x, base = exp(1)) {
  if(identical(base, exp(1))) {
    rray_log(x)
  }
  else {
    rray_log(x, base)
  }
}

# to check for dots
rray_trunc_vctrs_wrapper <- function(x, ...) {
  n_dots <- length(list(...))
  if (n_dots > 0L) {
    glubort("`trunc()` for rrays does not support arguments passed to `...`.")
  }

  rray_trunc(x)
}

# ------------------------------------------------------------------------------
# Summary group generic wrappers

rray_all_vctrs_wrapper <- function(x, na.rm) {
  vec_math_base("all", x, na.rm = na.rm)
}

rray_any_vctrs_wrapper <- function(x, na.rm) {
  vec_math_base("any", x, na.rm = na.rm)
}

rray_range_vctrs_wrapper <- function(x, na.rm) {
  vec_math_base("range", x, na.rm = na.rm)
}

rray_prod_vctrs_wrapper <- function(x, na.rm) {
  vec_math_base("prod", x, na.rm = na.rm)
}

rray_sum_vctrs_wrapper <- function(x, na.rm) {
  vec_math_base("sum", x, na.rm = na.rm)
}

# ------------------------------------------------------------------------------
# Math group generic wrappers

rray_cummax_vctrs_wrapper <- function(x) {
  vec_math_base("cummax", x)
}

rray_cummin_vctrs_wrapper <- function(x) {
  vec_math_base("cummin", x)
}

rray_cumsum_vctrs_wrapper <- function(x) {
  vec_math_base("cumsum", x)
}

rray_cumprod_vctrs_wrapper <- function(x) {
  vec_math_base("cumprod", x)
}

# ------------------------------------------------------------------------------
# Technically min() and max() are in the math generic too, but there is a
# min.vctrs_vctr method that we need to override, unlike the other functions

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
  vec_math_base("min", x, na.rm = na.rm)
}

#' @export
max.vctrs_rray <- function(x, ..., na.rm = FALSE) {
  vec_math_base("max", x, na.rm = na.rm)
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

#' @export
determinant.vctrs_rray <- function(x, logarithm = TRUE, ...) {
  determinant(as.matrix(x), logarithm = logarithm, ...)
}
