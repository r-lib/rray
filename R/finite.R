# No need to export these, but they flow through to `vec_math()`

rray_is_nan <- function(x) {
  vec_cast_container(vec_math_base("is.nan", x), x)
}

rray_is_finite <- function(x) {
  vec_cast_container(vec_math_base("is.finite", x), x)
}

rray_is_infinite <- function(x) {
  vec_cast_container(vec_math_base("is.infinite", x), x)
}
