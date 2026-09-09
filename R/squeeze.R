#' Squeeze axes of an array
#'
#' `rray_squeeze()` drops axes with a dimension of 1.
#'
#' @details
#' Only dimensions and dimension names change. Squeezed axes lose their names.
#' Surviving axes keep their names and carry them to their new positions.
#'
#' @param x An array.
#'
#' @param axes An integer vector of axes to squeeze. Each selected axis must
#'   have a dimension of 1.
#'
#' @returns
#' An array with the selected `axes` removed. If every axis is removed, the
#' result is a one-dimensional array with a dimension of 1.
#'
#' @export
#' @examples
#' x <- array(1:10, c(10, 1, 1))
#'
#' rray_squeeze(x, 2)
#' rray_squeeze(x, c(2, 3))
rray_squeeze <- function(x, axes) {
  .Call(ffi_rray_squeeze, x, axes, environment())
}
