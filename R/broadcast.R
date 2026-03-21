#' Broadcast an array to new dimension sizes
#'
#' @description
#' `rray_broadcast()` broadcasts an array to a new set of dimension sizes using
#' tidyverse recycling rules. Each dimension of `x` must either match the
#' corresponding target dimension or be 1, in which case it is repeated to fill
#' the target.
#'
#' New dimensions can be added by supplying a `dimension_sizes` with greater
#' dimensionality than `x` has. For example, a 2x3 array can be broadcast to a
#' 2x3x4 array.
#'
#' @param x An array.
#' @param dimension_sizes An integer vector of target dimension sizes.
#'
#' @returns
#' An array with the shape specified by `dimension_sizes`.
#'
#' @export
#' @examples
#' # Broadcast a vector to a matrix
#' rray_broadcast(1:3, c(3L, 2L))
#'
#' # Broadcast a row to fill a matrix
#' rray_broadcast(array(1:2, c(1L, 2L)), c(3L, 2L))
#'
#' # Add a new dimension
#' rray_broadcast(array(1:6, c(2L, 3L)), c(2L, 3L, 4L))
rray_broadcast <- function(x, dimension_sizes) {
  .Call(ffi_rray_broadcast, x, dimension_sizes, environment())
}
