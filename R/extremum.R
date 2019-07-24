#' Maximum and minimum values
#'
#' @description
#'
#' `rray_maximum()` and `rray_minimum()` compute the elementwise max / min
#' between `x` and `y`.
#'
#' @param x,y A vector, matrix, array or rray.
#'
#' @return
#'
#' The elementwise max/min of `x` and `y`, with broadcasting.
#'
#' @examples
#' # Elementwise maximum
#' rray_maximum(c(1, 2, 3), c(3, 2, 1))
#'
#' # Elementwise minimum
#' rray_minimum(c(1, 2, 3), c(3, 2, 1))
#'
#' # With broadcasting
#' x <- matrix(1:3)
#' rray_maximum(x, t(x))
#'
#' @name extremum
#' @export
rray_maximum <- function(x, y) {
  out <- rray__maximum(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
}

#' @rdname extremum
#' @export
rray_minimum <- function(x, y) {
  out <- rray__minimum(x, y)
  container <- vec_ptype_container2(x, y)
  vec_cast_container(out, container)
}
