#' Split an array along axes
#'
#' @description
#' `rray_split()` splits `x` along the specified `axes`, returning a
#' list of subarrays. The dimensionality of `x` is retained in each
#' subarray, with the split axes collapsed to a dimension of 1.
#'
#' @param x An array.
#'
#' @param axes An integer vector of axes to split along. The number of resulting
#'   subarrays is the product of the dimensions along `axes`.
#'
#' @returns
#' A list of arrays, each with the same dimensionality as `x` but
#' with a dimension of 1 along the split `axes`.
#'
#' @export
#' @examples
#' x <- array(1:24, c(4, 3, 2))
#'
#' # Split along the 3rd axis
#' # (4, 3, 2) -> two (4, 3, 1) arrays
#' rray_split(x, 3)
#'
#' # Split along the 1st axis
#' # (4, 3, 2) -> four (1, 3, 2) arrays
#' rray_split(x, 1)
#'
#' # Split along multiple axes
#' # (4, 3, 2) -> twelve (1, 1, 2) arrays
#' rray_split(x, c(1, 2))
rray_split <- function(x, axes) {
  .Call(ffi_rray_split, x, axes, environment())
}
