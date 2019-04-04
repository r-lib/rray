#' @export
vec_math.vctrs_rray <- function(fun, x, ...) {

  fun <- switch(
    fun,
    "abs" = vec_math_abs
  )

  fun(x, ...)
}

vec_math_abs <- function(x, ...) {
  rray_abs(x)
}
