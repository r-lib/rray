#' Find the capacity of an array
#'
#' `rray_capacity()` returns the total number of elements in an array,
#' computed as the product of its dimension sizes.
#'
#' @param x An array.
#'
#' @returns
#' A single double representing the total number of elements.
#'
#' @export
#' @examples
#' rray_capacity(1:5)
#' rray_capacity(array(1, c(2, 3)))
#' rray_capacity(array(1, c(2, 3, 4)))
rray_capacity <- function(x) {
  .Call(ffi_rray_capacity, x, environment())
}
