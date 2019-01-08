#' @export
vec_math.vctrs_rray <- function(fun, x, ...) {
  res <- rray_unary_op_cpp(fun, x)
  res <- rray_restore(res, x)
  dim_names(res) <- dim_names(x)
  res
}
