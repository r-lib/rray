#' Broadcast an array to new dimensions
#'
#' @description
#' `rray_broadcast()` broadcasts an array to a new set of dimensions using
#' tidyverse recycling rules. Each dimension of `x` must either match the
#' corresponding target dimension or be 1, in which case it is repeated to fill
#' the target.
#'
#' Dimensionality can be expanded by supplying `dimensions` with greater
#' dimensionality than `x` has. For example, a 2x3 array can be broadcast to a
#' 2x3x4 array.
#'
#' @param x An array.
#' @param dimensions An integer vector of target dimensions.
#'
#' @returns
#' An array with dimensions of `dimensions`.
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
rray_broadcast <- function(x, dimensions) {
  .Call(ffi_rray_broadcast, x, dimensions, environment())
}

#' Broadcast arrays to common dimensions
#'
#' @description
#' `rray_broadcast_common()` broadcasts every array to the common dimensions
#' found by [rray_dimensions_common()].
#'
#' @param ... Arrays to broadcast.
#'
#' @param .dimensions If provided, an integer vector of dimensions to
#'   broadcast to, rather than computing common dimensions from `...`.
#'
#' @returns
#' A list of arrays, all with the common dimensions.
#'
#' @export
#' @examples
#' rray_broadcast_common(array(1:3, c(3L, 1L)), array(1:2, c(1L, 2L)))
#'
#' # Names of `...` are kept
#' rray_broadcast_common(x = 1:3, y = 1L)
#'
#' # `.dimensions` overrides the common dimensions
#' rray_broadcast_common(1:3, .dimensions = c(3L, 2L))
rray_broadcast_common <- function(..., .dimensions = NULL) {
  .Call(ffi_rray_broadcast_common, list2(...), .dimensions, environment())
}
