#' Find the dimensions of an array
#'
#' `rray_dimensions()` returns the dimension of each axis of an
#' array.
#'
#' @details
#' For a plain vector without a `dim` attribute, this returns its
#' length as a single integer (i.e., a 1-dimensional result).
#'
#' @param x An array.
#'
#' @returns
#' An integer vector of dimensions.
#'
#' @export
#' @examples
#' rray_dimensions(1:5)
#' rray_dimensions(array(1, c(2, 3)))
#' rray_dimensions(array(1, c(2, 3, 4)))
rray_dimensions <- function(x) {
  .Call(ffi_rray_dimensions, x, environment())
}
