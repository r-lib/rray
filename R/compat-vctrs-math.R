#' @export
vec_math.vctrs_rray <- function(.fn, .x, ...) {
  out <- vec_math_base(.fn, .x, ...)
  dim <- rray_dim(out)

  new_rray(
    out,
    size = dim[1L],
    shape = dim[-1L],
    dim_names = rray_dim_names(out)
  )
}
