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

    glubort("Unary math function not known: {fun}.")
  )
}

rray_log_vctrs_wrapper <- function(x, base = exp(1)) {
  if(identical(base, exp(1))) {
    rray_log(x)
  }
  else {
    rray_log(x, base)
  }
}
