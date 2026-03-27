#' Find the size of an array
#'
#' `rray_size()` returns the total number of elements in an array,
#' computed as the product of its dimensions.
#'
#' @param x An array.
#'
#' @returns
#' A single double representing the total number of elements.
#'
#' @export
#' @examples
#' rray_size(1:5)
#' rray_size(array(1, c(2, 3)))
#' rray_size(array(1, c(2, 3, 4)))
rray_size <- function(x) {
  .Call(ffi_rray_size, x, environment())
}
