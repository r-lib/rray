#' Permute array axes
#'
#' `rray_permute_axes()` reorders the axes of an array.
#'
#' @details
#' Names and titles travel with their axis to its new position.
#'
#' `rray_permute_axes(x, c(2, 1))` transposes a matrix, like [t()].
#'
#' @param x An array.
#'
#' @param axes The axes of `x` in their new order, given
#'   [by position or by title][faq-axes]. It must use each axis of `x` exactly
#'   once.
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
#' # Transpose a matrix, by title
#' z <- rray_set_titles(x, c("row", "col"))
#' rray_permute_axes(z, c("col", "row"))
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
