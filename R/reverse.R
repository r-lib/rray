#' Reverse elements along axes
#'
#' `rray_reverse()` reverses the order of the elements of `x` along one or more
#' `axes`.
#'
#' @details
#' Names on a reversed axis move with the data. Every other axis keeps its names
#' untouched.
#'
#' @param x An array.
#'
#' @param axes An integer vector of axes to reverse along.
#'
#' @returns
#' An array with the same type and dimensions as `x`.
#'
#' @export
#' @examples
#' rray_reverse(1:5, axes = 1)
#'
#' x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))
#'
#' # Reverse the rows, names move with the data
#' rray_reverse(x, axes = 1)
#'
#' # Reverse the columns
#' rray_reverse(x, axes = 2)
#'
#' # Reverse both axes
#' rray_reverse(x, axes = c(1, 2))
rray_reverse <- function(x, axes) {
  .Call(ffi_rray_reverse, x, axes, environment())
}
