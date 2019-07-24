#' Reshape an array
#'
#' `rray_reshape()` is similar to `dim()<-`. It reshapes `x` in such a way
#' that the dimensions are different, but the total size of the array is still
#' the same (as measured by [rray_elems()]).
#'
#' @param x A vector, matrix, array or rray.
#' @param dim An integer vector. The dimension to reshape to.
#'
#' @return
#'
#' `x` reshaped to the new dimensions of `dim`.
#'
#' @examples
#'
#' x <- matrix(1:6, ncol = 1)
#'
#' # Reshape with the same dimensionality
#' rray_reshape(x, c(2, 3))
#'
#' # Change the dimensionality and the dimensions
#' rray_reshape(x, c(3, 2, 1))
#'
#' # You cannot reshape to a total size that is
#' # different from the current size.
#' try(rray_reshape(x, c(6, 2)))
#'
#' # Note that you can broadcast to these dimensions!
#' rray_broadcast(x, c(6, 2))
#'
#' @export
rray_reshape <- function(x, dim) {
  dim <- vec_cast(dim, integer())

  res <- rray__reshape(x, dim)

  vec_cast_container(res, x)
}
