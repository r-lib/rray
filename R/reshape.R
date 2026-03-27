#' Reshape an array
#'
#' @description
#' `rray_reshape()` reshapes `x` to a new set of dimensions without
#' changing the total number of elements. Unlike [rray_broadcast()], which
#' repeats elements to fill new dimensions, `rray_reshape()` simply rearranges
#' the existing elements into a new shape without changing its size.
#'
#' @param x An array.
#' @param dimensions An integer vector of new dimensions.
#'
#' @returns
#' An array with the shape specified by `dimensions` and the same
#' size as `x`.
#'
#' @export
#' @examples
#' x <- 1:6
#'
#' # Reshape a vector into a matrix
#' rray_reshape(x, c(2L, 3L))
#'
#' # Reshape a vector into a 3D array
#' rray_reshape(x, c(3L, 2L, 1L))
#'
#' # Reshaping can't change the size
#' try(rray_reshape(x, c(6L, 2L)))
rray_reshape <- function(x, dimensions) {
  .Call(ffi_rray_reshape, x, dimensions, environment())
}
