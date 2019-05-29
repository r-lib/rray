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
  vec_cast_container(rray__sinh(x), x)
}

#' @rdname rray_sinh
#' @export
rray_cosh <- function(x) {
  vec_cast_container(rray__cosh(x), x)
}

#' @rdname rray_sinh
#' @export
rray_tanh <- function(x) {
  vec_cast_container(rray__tanh(x), x)
}

#' @rdname rray_sinh
#' @export
rray_asinh <- function(x) {
  vec_cast_container(rray__asinh(x), x)
}

#' @rdname rray_sinh
#' @export
rray_acosh <- function(x) {
  vec_cast_container(rray__acosh(x), x)
}

#' @rdname rray_sinh
#' @export
rray_atanh <- function(x) {
  vec_cast_container(rray__atanh(x), x)
}
