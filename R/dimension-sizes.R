#' Find the dimension sizes of an array
#'
#' `rray_dimension_sizes()` returns the size of each dimension of an
#' array.
#'
#' @details
#' For a plain vector without a `dim` attribute, this returns its
#' length as a single integer (i.e., a 1-dimensional result).
#'
#' @param x An array.
#'
#' @returns
#' An integer vector of dimension sizes.
#'
#' @export
#' @examples
#' rray_dimension_sizes(1:5)
#' rray_dimension_sizes(array(1, c(2, 3)))
#' rray_dimension_sizes(array(1, c(2, 3, 4)))
rray_dimension_sizes <- function(x) {
  .Call(ffi_rray_dimension_sizes, x)
}
