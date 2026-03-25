#' Sum an array along axes
#'
#' @description
#' `rray_sum()` computes the sum along the specified `axes`. The
#' dimensionality of `x` is retained in the result, with the reduced
#' axes collapsed to size 1.
#'
#' @details
#' If summing an integer array would overflow, an error is thrown.
#'
#' @param x An array.
#'
#' @param axes An integer vector of axes to reduce over. `1` reduces
#'   rows, `2` reduces columns, and so on.
#'
#' @returns
#' An array with the same dimensionality as `x`, but with the
#' dimension sizes along `axes` reduced to 1.
#'
#' @export
#' @examples
#' x <- array(1:10, c(5L, 2L))
#'
#' # Sum along rows
#' rray_sum(x, 1L)
#'
#' # Sum along columns
#' rray_sum(x, 2L)
#'
#' # Sum along both axes
#' rray_sum(x, c(1L, 2L))
rray_sum <- function(x, axes) {
  .Call(ffi_rray_sum, x, axes, environment())
}
