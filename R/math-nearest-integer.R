#' Rounding functions
#'
#' @description
#'
#' - `rray_ceiling()` - Compute the smallest integer value not less than `x`.
#'
#' - `rray_floor()` - Compute the largest integer value not greater than `x`.
#'
#' - `rray_trunc()` - Compute the nearest integer value not greater
#'  _in magnitude_ than `x`.
#'
#' - `rray_round()` - Round `x` to the specified number of decimal places.
#' Rounding is done _towards the even digit_, see [base::round()] for more
#' information.
#'
#' - `rray_signif()` - Round `x` to the specified number of significant digits.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @examples
#' x <- matrix(c(2.22, 4.5, 5.5))
#'
#' # Round up
#' rray_ceiling(x)
#'
#' # Round down
#' rray_floor(x)
#'
#' # Truncate towards 0
#' rray_trunc(x)
#' rray_trunc(-x)
#'
#' # Round to the specified number of decimal
#' # places. Border values (on a `5`) are rounded
#' # towards even numbers
#' rray_round(x)
#' rray_round(-x)
#'
#' rray_round(x, digits = 1)
#'
#' # Two significant digits
#' rray_signif(x, 2)
#'
#' @family nearest integer math functions
#' @export
rray_ceiling <- function(x) {
  rray_math_unary_base(rray__ceiling, x)
}

#' @rdname rray_ceiling
#' @export
rray_floor <- function(x) {
  rray_math_unary_base(rray__floor, x)
}

#' @rdname rray_ceiling
#' @export
rray_trunc <- function(x) {
  rray_math_unary_base(rray__trunc, x)
}

#' @rdname rray_ceiling
#' @export
rray_round <- function(x, digits = 0) {
  rray_math_unary_base_raw(round, x, digits = digits)
}

#' @rdname rray_ceiling
#' @export
rray_signif <- function(x, digits = 6) {
  rray_math_unary_base_raw(signif, x, digits = digits)
}
