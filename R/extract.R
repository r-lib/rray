#' Extract values from an array
#'
#' `rray_extract()` selects values from `x` by 1D location or by coordinate
#' point. The result is always a 1D array.
#'
#' @details
#' The type and shape of `i` decide how it is used:
#'
#' - An integer or double vector holds 1D locations. `x` is treated as its
#'   equivalent flattened array, so location `4` in a 2 x 3 matrix is row 2,
#'   column 2. Locations follow the usual R rules: negative values drop
#'   elements, zero is ignored, duplicates repeat elements, and `NA` gives a
#'   missing value.
#'
#' - A logical vector must be size 1 or the size of `x`. A logical array must
#'   have the same dimensions as `x`, so `rray_extract(x, x > 5)` works. When
#'   `TRUE`, the corresponding value in `x` is returned.
#'
#' - An integer or double matrix holds coordinate points. Each row selects one
#'   element, and there must be one column per axis of `x`. Coordinates must be
#'   positive and within the dimension of their axis. `NA` at any position
#'   within the row generates a missing value.
#'
#' For raw arrays the missing value is `as.raw(0)`, and for list arrays it is
#' `NULL`.
#'
#' @param x An array.
#'
#' @param i A numeric vector of locations, a logical vector, a logical array
#'   with the same dimensions as `x`, or a numeric matrix of coordinate points.
#'
#' @returns
#' A one-dimensional array. Names are always dropped.
#'
#' @export
#' @examples
#' x <- array(1:24, c(2L, 3L, 4L))
#'
#' # 1D locations
#' rray_extract(x, c(1, 4, 24))
#'
#' # Negative locations drop elements
#' rray_extract(x, -(1:20))
#'
#' # A logical mask with the same dimensions as `x`
#' rray_extract(x, x %% 5 == 0)
#'
#' # One row per point, one column per axis
#' points <- rbind(
#'   c(1, 1, 1),
#'   c(2, 3, 4),
#'   c(1, 2, 3)
#' )
#' rray_extract(x, points)
rray_extract <- function(x, i) {
  .Call(ffi_rray_extract, x, i, environment())
}
