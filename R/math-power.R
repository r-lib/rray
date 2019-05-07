#' Power functions
#'
#' @description
#'
#' - `rray_square()` - computes the elementwise square of `x` (i.e. `x * x`).
#'
#' - `rray_cube()` - computes the elementwise cube of `x` (i.e. `x * x * x`).
#'
#' - `rray_sqrt()` - computes the elementwise square root of `x`.
#'
#' - `rray_cbrt()` - computes the elementwise cube root of `x`.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @examples
#' x <- matrix(c(2, 4, 6))
#'
#' rray_square(x)
#' rray_sqrt(x)
#'
#' rray_cube(x)
#' rray_cbrt(x)
#'
#' @family power math functions
#' @export
rray_square <- function(x) {
  rray_math_unary_base(rray__square, x)
}

#' @rdname rray_square
#' @export
rray_cube <- function(x) {
  rray_math_unary_base(rray__cube, x)
}

#' @rdname rray_square
#' @export
rray_sqrt <- function(x) {
  rray_math_unary_base(rray__sqrt, x)
}

#' @rdname rray_square
#' @export
rray_cbrt <- function(x) {
  rray_math_unary_base(rray__cbrt, x)
}

# ------------------------------------------------------------------------------

