#' Extract or assign values in an array
#'
#' @description
#' `rray_extract()` selects values from `x` by 1D location or by coordinate
#' point. The result is always a 1D array.
#'
#' `rray_extract_assign()` replaces the selected values with `value` and returns
#' a modified copy of `x`.
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
#' @section Assignment:
#' `rray_extract_assign()` selects values with the same rules, with one
#' exception: `i` can't contain missing values.
#'
#' `value` is cast to the type of `x`, then broadcast to the number of selected
#' values. It must be size 1 or have one element per selected value.
#'
#' When a value is selected more than once, the last assignment wins:
#'
#' ```r
#' rray_extract_assign(1:3, c(2, 2), c(10L, 20L))
#' #> [1]  1 20  3
#' ```
#'
#' @param x An array.
#'
#' @param i A numeric vector of locations, a logical vector, a logical array
#'   with the same dimensions as `x`, or a numeric matrix of coordinate points.
#'
#' @param value An array to assign to the selected values.
#'
#' @returns
#' - `rray_extract()` returns a one-dimensional array. Names are always dropped.
#'
#' - `rray_extract_assign()` returns `x` with the selected values replaced. The
#'   type, dimensions, and names of `x` are kept.
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
#'
#' # Assign one value to every selected element
#' rray_extract_assign(x, x %% 5 == 0, 0L)
#'
#' # Or one value per selected element
#' rray_extract_assign(x, points, c(100L, 200L, 300L))
rray_extract <- function(x, i) {
  .Call(ffi_rray_extract, x, i, environment())
}

#' @rdname rray_extract
#' @export
rray_extract_assign <- function(x, i, value) {
  .Call(ffi_rray_extract_assign, x, i, value, environment())
}
