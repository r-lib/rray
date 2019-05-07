#' Hyperbolic functions
#'
#' @description
#'
#' - `rray_sinh()` - Hyperbolic sine
#'
#' - `rray_cosh()` - Hyperbolic cosine
#'
#' - `rray_tanh()` - Hyperbolic tangent
#'
#' - `rray_asinh()` - Hyperbolic arc-sine
#'
#' - `rray_acosh()` - Hyperbolic arc-cosine
#'
#' - `rray_atanh()` - Hyperbolic arc-tangent
#'
#' @param x A vector, matrix, array or rray.
#'
#' @examples
#' x <- matrix(c(2, 4, 6))
#'
#' rray_sinh(x)
#' rray_cosh(x)
#' rray_tanh(x)
#'
#' @family hyperbolic math functions
#' @export
rray_sinh <- function(x) {
  rray_math_unary_base(rray__sinh, x)
}

#' @rdname rray_sinh
#' @export
rray_cosh <- function(x) {
  rray_math_unary_base(rray__cosh, x)
}

#' @rdname rray_sinh
#' @export
rray_tanh <- function(x) {
  rray_math_unary_base(rray__tanh, x)
}

#' @rdname rray_sinh
#' @export
rray_asinh <- function(x) {
  rray_math_unary_base(rray__asinh, x)
}

#' @rdname rray_sinh
#' @export
rray_acosh <- function(x) {
  rray_math_unary_base(rray__acosh, x)
}

#' @rdname rray_sinh
#' @export
rray_atanh <- function(x) {
  rray_math_unary_base(rray__atanh, x)
}
