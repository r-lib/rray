#' Maximum and minimum values
#'
#' @description
#'
#' `rray_maximum()` and `rray_minimum()` compute the elementwise max / min
#' between `x` and `y`.
#'
#' @param x,y A vector, matrix, array or rray.
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
#' @family math functions
#' @export
rray_maximum <- function(x, y) {
  out <- rray__maximum(x, y)
  container <- vec_type_container2(x, y)
  vec_cast_container(out, container)
}

#' @rdname rray_maximum
#' @export
rray_minimum <- function(x, y) {
  out <- rray__minimum(x, y)
  container <- vec_type_container2(x, y)
  vec_cast_container(out, container)
}

# ------------------------------------------------------------------------------

#' Fused multiply-add
#'
#' `rray_multiply_add()` computes `x * y + z`, with broadcasting.
#' It is more efficient than simply doing those operations in sequence.
#'
#' @param x,y,z A vector, matrix, array or rray.
#'
#' @examples
#' # 2 * 3 + 5
#' rray_multiply_add(2, 3, 5)
#'
#' # Using broadcasting
#' rray_multiply_add(matrix(1:5), matrix(1:2, nrow = 1L), 3L)
#'
#' # ^ Equivalent to:
#' x <- matrix(rep(1:5, 2), ncol = 2)
#' y <- matrix(rep(1:2, 5), byrow = TRUE, ncol = 2)
#' z <- matrix(3L, nrow = 5, ncol = 2)
#' x * y + z
#'
#' @family math functions
#' @export
rray_multiply_add <- function(x, y, z) {
  out <- rray__multiply_add(x, y, z)
  container <- vec_type_container_common(x, y, z)
  vec_cast_container(out, container)
}
