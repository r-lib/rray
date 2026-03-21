#' Find the dimensionality of an object
#'
#' `rray_dimensionality()` returns the number of dimensions of an object as
#' a single integer.
#'
#' @param x An object.
#'
#' @returns
#' A single integer representing the number of dimensions.
#'
#' @export
#' @examples
#' rray_dimensionality(1)
#' rray_dimensionality(array(1, c(2, 3)))
#' rray_dimensionality(array(1, c(2, 3, 4)))
rray_dimensionality <- function(x) {
  .Call(ffi_rray_dimensionality, x)
}
