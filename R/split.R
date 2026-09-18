#' Split an array along an axis
#'
#' @description
#' `rray_split()` divides `x` into contiguous arrays along `axis`. Every array
#' keeps the dimensionality of `x`, only the dimension along `axis` changes.
#'
#' Combining the arrays along the same axis reconstructs `x`.
#'
#' @param x An array.
#'
#' @param axis A single integer between 1 and the dimensionality of `x`.
#'
#' @param dimensions An integer vector describing the dimension of each array
#'   along `axis`. One of:
#'
#'   - A single value, used as the dimension of every array. It must be
#'     positive and must evenly divide the dimension of `x` along `axis`.
#'
#'   - A vector of any other length, giving the dimension of each array
#'     directly. The values must be non-negative and must sum to the dimension
#'     of `x` along `axis`.
#'
#' @returns
#' An unnamed list of arrays with the same dimensionality as `x`.
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
