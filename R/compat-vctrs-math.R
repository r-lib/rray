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
