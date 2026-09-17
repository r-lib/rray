#' Remove array axes
#'
#' `rray_remove_axes()` drops axes with a dimension of 1.
#'
#' @details
#' Removed axes lose their names.
#' Surviving axes keep their names and carry them to their new positions.
#'
#' @param x An array.
#'
#' @param axes An integer vector of axes to remove. Each selected axis must
#'   have a dimension of 1. At least one axis must remain.
#'
#' @returns
#' An array with the selected `axes` removed.
#'
#' @export
#' @examples
#' x <- array(1:10, c(10, 1, 1))
#'
#' rray_remove_axes(x, 2)
#' rray_remove_axes(x, c(2, 3))
rray_remove_axes <- function(x, axes) {
  .Call(ffi_rray_remove_axes, x, axes, environment())
}
