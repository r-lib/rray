#' Fused multiply-add
#'
#' `rray_multiply_add()` computes `x * y + z`, with broadcasting.
#' It is more efficient than simply doing those operations in sequence.
#'
#' @param x,y,z A vector, matrix, array or rray.
#'
#' @return
#'
#' An object of the common type of the inputs, containing the result of the
#' multiply-add operation.
#'
#' @examples
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
#' @export
rray_multiply_add <- function(x, y, z) {
  out <- rray__multiply_add(x, y, z)
  container <- vec_ptype_container_common(x, y, z)
  vec_cast_container(out, container)
}
