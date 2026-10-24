#' Remove array axes
#'
#' `rray_remove_axes()` removes axes with a dimension of 1.
#'
#' @details
#' Removed axes lose their names and titles.
#' Surviving axes keep their names and titles and carry them to their new
#' locations.
#'
#' @param x An array.
#'
#' @param axes The axes to remove, given [by position or by title][faq-axes].
#'   Each selected axis must have a dimension of 1. At least one axis must
#'   remain.
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
#'
#' y <- rray_set_titles(x, c("row", "col", "page"))
#' rray_remove_axes(y, c("col", "page"))
rray_remove_axes <- function(x, axes) {
  .Call(ffi_rray_remove_axes, x, axes, environment())
}
