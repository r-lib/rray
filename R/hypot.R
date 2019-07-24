#' Compute the square root of the sum of squares
#'
#' `rray_hypot()` computes the elementwise square root of the sum
#' of squares of `x` and `y`.
#'
#' @param x,y A vector, matrix, array or rray.
#'
#' @return
#'
#' An object of the common type of `x` and `y` containing the square root
#' of the sum of squares.
#'
#' @examples
#' x <- matrix(c(2, 4, 6))
#'
#' # With broadcasting
#' rray_hypot(x, t(x))
#'
#' @export
rray_hypot <- function(x, y) {
  out <- rray__hypot(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
}
