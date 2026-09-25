#' Extract values from an array
#'
#' `rray_extract()` selects values from `x` by 1D location or by coordinate
#' point. The result is always a 1D array.
#'
#' @details
#' The type and shape of `i` decide how it is used.
#'
#' - An integer or double vector holds flat positions. `x` is treated as its
#'   column-major storage vector, so position `4` in a 2 x 3 matrix is row 2,
#'   column 2. Positions follow the usual R rules: negative values drop
#'   elements, zero is ignored, duplicates repeat elements, and `NA` gives a
#'   missing value.
#'
#' - A logical vector is a flat mask. It must be size 1 or the size of `x`. A
#'   logical array is also a mask if it has the same dimensions as `x`, so
#'   `rray_extract(x, x > 5)` works.
#'
#' - An integer or double matrix holds coordinate points. Each row selects one
#'   element, and there must be one column per axis of `x`. Coordinates must be
#'   positive and within the dimension of their axis. `NA` gives a missing
#'   value.
#'
#' For raw arrays the missing value is `as.raw(0)`, and for list arrays it is
#' `NULL`.
#'
#' @param x A bare array or vector.
#'
#' @param i A numeric vector of locations, a logical vector, a logical array
#'   with the same dimensions as `x`, or a numeric matrix of coordinate points.
#'
#' @returns
#' A one-dimensional array with the same storage type as `x`.
#'
#' @export
#' @examples
#' x <- array(1:24, c(2L, 3L, 4L))
#'
#' # Flat positions, in column-major order
#' rray_extract(x, c(1, 4, 24))
#'
#' # Negative positions drop elements
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
