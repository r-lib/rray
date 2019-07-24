#' Create a matrix with `x` on the diagonal
#'
#' `rray_diag()` creates a matrix filled with `x` on the diagonal. Use `offset`
#' to place `x` along an offset from the diagonal.
#'
#' @details
#'
#' No dimension names will be on the result.
#'
#' @param x A vector, matrix, array or rray.
#'
#' @param offset A single integer specifying the offset from the diagonal to
#' place `x`. This can be positive or negative.
#'
#' @return
#'
#' A matrix, with `x` on the diagonal.
#'
#' @examples
#' # Creates a diagonal matrix
#' rray_diag(1:5)
#'
#' # Offset `1:5` by 1
#' rray_diag(1:5, 1)
#'
#' # You can also go the other way
#' rray_diag(1:5, -1)
#'
#' # Identity matrix
#' rray_diag(rep(1, 5))
#'
#' # One interesting use case of this is to create
#' # a square empty matrix with dimensions (offset, offset)
#' rray_diag(rray(integer()), 3)
#' rray_diag(logical(), 3)
#'
#' @export
rray_diag <- function(x, offset = 0) {

  if (length(offset) != 1) {
    glubort("`offset` must have length 1, not {length(offset)}.")
  }

  offset <- vec_cast(offset, integer())

  res <- rray__diag(x, offset)

  # no dim names on the result

  vec_cast_container(res, x)
}
