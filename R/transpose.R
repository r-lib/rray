#' Transpose an array
#'
#' `rray_transpose()` reorders the axes of an array. By default it reverses
#' them, which is the usual transpose of a matrix.
#'
#' @details
#' Names travel with their axis to its new position.
#'
#' Unlike [t()], a 1D array comes back unchanged rather than becoming a matrix.
#' Reversing a single axis gives that axis back.
#'
#' @param x An array.
#'
#' @param permutation An integer vector, or `NULL`. Axis `i` of the result is
#'   taken from axis `permutation[i]` of `x`, so it must use each axis of `x`
#'   exactly once. If `NULL`, the axes are reversed.
#'
#' @returns
#' An array with the axes of `x` reordered by `permutation`.
#'
#' @export
#' @examples
#' x <- array(1:6, c(2, 3))
#' rray_transpose(x)
#'
#' y <- array(1:24, c(4, 3, 2))
#'
#' # Reverse every axis
#' # (4, 3, 2) -> (2, 3, 4)
#' rray_dimensions(rray_transpose(y))
#'
#' # Swap the first two axes, leaving the third alone
#' # (4, 3, 2) -> (3, 4, 2)
#' rray_dimensions(rray_transpose(y, c(2, 1, 3)))
#'
#' # A 1D array comes back unchanged
#' rray_transpose(1:3)
rray_transpose <- function(x, permutation = NULL) {
  .Call(ffi_rray_transpose, x, permutation, environment())
}
