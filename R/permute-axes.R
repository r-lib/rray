#' Permute array axes
#'
#' `rray_permute_axes()` reorders the axes of an array.
#'
#' @details
#' Names travel with their axis to its new location.
#'
#' `rray_permute_axes(x, c(2, 1))` transposes a matrix, like [t()].
#'
#' @param x An array.
#'
#' @param axes An integer vector. It must use each axis of `x` exactly once.
#'
#' @returns
#' An array with the axes of `x` reordered by `axes`.
#'
#' @seealso [rray_move_axes()]
#'
#' @export
#' @examples
#' x <- array(1:6, c(2, 3))
#'
#' # Transpose a matrix
#' rray_permute_axes(x, c(2, 1))
#'
#' y <- array(1:24, c(4, 3, 2))
#'
#' # Reverse every axis
#' # (4, 3, 2) -> (2, 3, 4)
#' rray_dimensions(rray_permute_axes(y, c(3, 2, 1)))
#'
#' # Swap the first two axes, leaving the third alone
#' # (4, 3, 2) -> (3, 4, 2)
#' rray_dimensions(rray_permute_axes(y, c(2, 1, 3)))
rray_permute_axes <- function(x, axes) {
  .Call(ffi_rray_permute_axes, x, axes, environment())
}
