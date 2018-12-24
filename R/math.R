#' @export
vec_math.vctrs_rray <- function(fun, x, ...) {
  vec_restore(rray_unary_op_cpp(fun, x), x)
}
