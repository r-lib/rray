#' Reshape an array
#'
#' @description
#' `rray_reshape()` reshapes `x` to a new set of dimension sizes without
#' changing the total number of elements. Unlike [rray_broadcast()], which
#' repeats elements to fill new dimensions, `rray_reshape()` simply rearranges
#' the existing elements into a new shape without changing its size.
#'
#' @param x An array.
#' @param dimension_sizes An integer vector of new dimension sizes.
#'
#' @returns
#' An array with the shape specified by `dimension_sizes` and the same
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
rray_reshape <- function(x, dimension_sizes) {
  .Call(ffi_rray_reshape, x, dimension_sizes, environment())
}
