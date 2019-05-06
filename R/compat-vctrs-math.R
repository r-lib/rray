#' @export
vec_math.vctrs_rray <- function(fun, x, ...) {
  f <- rray_math_unary_op_switch(fun)
  f(x, ...)
}

rray_math_unary_op_switch <- function(fun) {
  switch(
    fun,
    "abs" = rray_abs,
    glubort("Unary math function not known: {fun}.")
  )
}
