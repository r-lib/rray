#' Split an array along an axis
#'
#' @description
#' `rray_split()` divides `x` into contiguous chunks along `axis`. Every chunk
#' keeps the dimensionality of `x`, only the dimension along `axis` changes.
#'
#' `sizes` describes those chunks in one of two ways:
#'
#' - A single value is the dimension of every chunk. It must be positive and
#'   must evenly divide the dimension of `x` along `axis`.
#'
#' - A vector of any other length gives the dimension of each chunk directly.
#'   The values must be non-negative and must sum to the dimension of `x` along
#'   `axis`.
#'
#' Combining the chunks along the same axis reconstructs `x`.
#'
#' @param x An array.
#'
#' @param axis A single integer between 1 and the dimensionality of `x`.
#'
#' @param sizes An integer vector of chunk dimensions along `axis`.
#'
#' @returns
#' An unnamed list of arrays with the same dimensionality as `x`.
#'
#' @export
#' @examples
#' x <- array(1:12, c(6, 2))
#'
#' # Three chunks of two rows
#' rray_split(x, 1, 2)
#'
#' # Chunks of one and five rows
#' rray_split(x, 1, c(1, 5))
#'
#' # One chunk per column
#' rray_split(x, 2, 1)
#'
#' # Chunks of dimension zero are allowed in the explicit form
#' rray_split(x, 2, c(0, 2))
#'
#' # Splitting and combining along the same axis are inverses
#' rray_combine(!!!rray_split(x, 1, 3), .axis = 1)
rray_split <- function(x, axis, sizes) {
  .Call(ffi_rray_split, x, axis, sizes, environment())
}
