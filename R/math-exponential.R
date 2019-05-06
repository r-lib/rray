#' Logarithms and exponentials
#'
#' @description
#'
#' `rray_exp()` computes the exponential of `x`.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @examples
#'
#' # Exponential
#' rray_exp(1L)
#' rray_exp(rray(1:5, c(5, 1)))
#'
#' @family exponential math functions
#' @export
rray_exp <- function(x) {
  rray_math_unary_base(rray__exp, x)
}
