# Math generics are:
# - cummax
# - cummin
# - cumsum
# - cumprod

# Used in `vec_math()`

rray_cummax_vctrs_wrapper <- function(x) {
  vec_math_base("cummax", vec_data(x))
}

rray_cummin_vctrs_wrapper <- function(x) {
  vec_math_base("cummin", vec_data(x))
}

rray_cumsum_vctrs_wrapper <- function(x) {
  vec_math_base("cumsum", vec_data(x))
}

rray_cumprod_vctrs_wrapper <- function(x) {
  vec_math_base("cumprod", vec_data(x))
}
