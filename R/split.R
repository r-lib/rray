#' Split an array along an axis
#'
#' @description
#' `rray_split()` divides `x` into contiguous arrays along an `axis`.
#'
#' Using `rray_combine()` along the same `axis` reconstructs `x`.
#'
#' @param x An array.
#'
#' @param axis A single integer representing the axis to split on.
#'
#' @param dimensions One of:
#'
#'   - A single positive integer, used as the dimension of every array. It must
#'     evenly divide the dimension of `x` along `axis`.
#'
#'   - A vector of positive (or zero) integers, giving the dimension of each
#'     array directly. They must sum to the dimension of `x` along `axis`.
#'
#' @returns
#' A list of arrays each with the same dimensions as `x`, except along `axis`.
#'
#' @export
#' @examples
#' x <- array(1:12, c(6, 2))
#'
#' # Three arrays of two rows
#' rray_split(x, 1, 2)
#'
#' # Arrays of one and five rows
#' rray_split(x, 1, c(1, 5))
#'
#' # One array per column
#' rray_split(x, 2, 1)
#'
#' # Arrays of dimension zero are allowed in the explicit form
#' rray_split(x, 2, c(0, 2))
#'
#' # Splitting and combining along the same axis are inverses
#' rray_combine(!!!rray_split(x, 1, 3), .axis = 1)
rray_split <- function(x, axis, dimensions) {
  .Call(ffi_rray_split, x, axis, dimensions, environment())
}
