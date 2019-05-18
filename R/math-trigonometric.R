#' Trigonometric functions
#'
#' @description
#'
#' - `rray_sin()` - Sine
#'
#' - `rray_cos()` - Cosine
#'
#' - `rray_tan()` - Tangent
#'
#' - `rray_asin()` - Arc-sine
#'
#' - `rray_acos()` - Arc-cosine
#'
#' - `rray_atan()` - Arc-tangent
#'
#' - `rray_atan2()` - Two argument arc-tangent
#'
#' - `rray_sinpi()` - Numerically accurate `rray_sin(pi * x)`
#'
#' - `rray_cospi()` - Numerically accurate `rray_cosin(pi * x)`
#'
#' - `rray_tanpi()` - Numerically accurate `rray_tan(pi * x)`
#'
#' @param x,y A vector, matrix, array or rray.
#'
#' @examples
#' x <- matrix(c(2, 4, 6))
#'
#' rray_sin(x)
#' rray_cos(x)
#' rray_tan(x)
#'
#' # rray_atan2() broadcasts
#' rray_atan2(matrix(1:5), matrix(1:5, nrow = 1))
#'
#' # ---------------------------------------------------------------------------
#' # Numerical accuracy of `sin(pi * x)` VS `sinpi(x)`
#' # From `?sin`
#'
#' x <- seq(-3, 7, by = 1)
#' cbind(
#'    x,
#'    sin(pi * x),
#'    rray_sin(pi * x),
#'    sinpi(x),
#'    rray_sinpi(x),
#'    cos(pi * x),
#'    rray_cos(pi * x),
#'    cospi(x),
#'    rray_cospi(x),
#'    tan(pi * x),
#'    rray_tan(pi * x),
#'    tanpi(x),
#'    rray_tanpi(x),
#'    deparse.level = 2
#' )
#'
#' @family trigonometric math functions
#' @export
rray_sin <- function(x) {
  rray_math_unary_base(rray__sin, x)
}

#' @rdname rray_sin
#' @export
rray_cos <- function(x) {
  rray_math_unary_base(rray__cos, x)
}

#' @rdname rray_sin
#' @export
rray_tan <- function(x) {
  rray_math_unary_base(rray__tan, x)
}

#' @rdname rray_sin
#' @export
rray_asin <- function(x) {
  rray_math_unary_base(rray__asin, x)
}

#' @rdname rray_sin
#' @export
rray_acos <- function(x) {
  rray_math_unary_base(rray__acos, x)
}

#' @rdname rray_sin
#' @export
rray_atan <- function(x) {
  rray_math_unary_base(rray__atan, x)
}

# atan2() is NOT an internal generic

#' @rdname rray_sin
#' @export
rray_atan2 <- function(y, x) {
  rray_math_binary_base_typed(rray__atan2, y, x, double())
}

# ------------------------------------------------------------------------------

# Not implemented in xtensor, but these are `vec_math()` generics that
# R implements and we need to have a `vec_math()` implementation for them

#' @rdname rray_sin
#' @export
rray_sinpi <- function(x) {
  rray_math_unary_base_raw(sinpi, x)
}

#' @rdname rray_sin
#' @export
rray_cospi <- function(x) {
  rray_math_unary_base_raw(cospi, x)
}

#' @rdname rray_sin
#' @export
rray_tanpi <- function(x) {
  rray_math_unary_base_raw(tanpi, x)
}

# ------------------------------------------------------------------------------

rray_math_unary_base_raw <- function(f, x, ...) {
  res <- f(vec_data_fast(x), ...)
  res <- new_array(res, dim = rray_dim(res), dimnames = rray_dim_names(x))
  vec_cast_container(res, x)
}

rray_math_binary_base_typed <- function(f, x, y, type) {
  rray_arith_binary_base_typed(f, x, y, type)
}
