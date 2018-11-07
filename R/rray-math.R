rray_math_core <- function(fun, x, ...) {
  math_fun <- try(get(paste0("rray_", fun, "_cpp")), silent = TRUE)

  if (inherits(math_fun, "try-error")) {
    glubort("{fun} not yet implemented")
  }

  math_fun(x)
}

#' @export
vec_math.vctrs_rray <- function(fun, x, ...) {
  vec_restore(rray_math_core(fun, x, ...), x)
}
