#' @export
vec_math.vctrs_rray <- function(fun, x, ...) {
  res <- rray_op_unary_cpp(fun, x)
  dim_names(res) <- dim_names(x)
  vec_restore(res, x)
}
