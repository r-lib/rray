#' Locate the maximum or minimum along an axis
#'
#' @description
#' - `rray_locate_max()` finds the position of the maximum along `axis`.
#'
#' - `rray_locate_min()` finds the position of the minimum along `axis`.
#'
#' @details
#' The dimensionality of `x` is retained, with `axis` collapsed to a dimension
#' of 1.
#'
#' Ties return the first position, like [which.max()] and [which.min()]:
#'
#' ```r
#' rray_locate_max(c(1, 3, 3), 1L) # 2
#' ```
#'
#' Missing values are infectious, so the result is `NA` if any value along
#' `axis` is `NA` or `NaN`. Use `na_rm = TRUE` to skip them:
#'
#' ```r
#' rray_locate_max(c(1, NaN, 3), 1L) # NA
#' rray_locate_max(c(1, NaN, 3), 1L, na_rm = TRUE) # 3
#' ```
#'
#' When there is no position to return, the result is `NA`. This happens when
#' `axis` has dimension 0, or when every value is missing and `na_rm = TRUE`:
#'
#' ```r
#' rray_locate_max(c(NA, NA), 1L, na_rm = TRUE) # NA
#' ```
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x An array.
#'
#' @param axis A single integer giving the axis to locate along.
#'
#' @param na_rm If `TRUE`, missing values are skipped.
#'
#' @returns
#' An integer array with the same dimensionality as `x`, but with the dimension
#' along `axis` reduced to 1.
#'
#' @name rray-locate
#' @examples
#' x <- array(c(3L, 1L, 2L, 4L, 6L, 5L), c(3L, 2L))
#'
#' # Position of the maximum going down the rows
#' rray_locate_max(x, 1L)
#'
#' # Position of the minimum going across the columns
#' rray_locate_min(x, 2L)
NULL

#' @rdname rray-locate
#' @export
rray_locate_max <- function(x, axis, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_locate_max, x, axis, na_rm, environment())
}

#' @rdname rray-locate
#' @export
rray_locate_min <- function(x, axis, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_locate_min, x, axis, na_rm, environment())
}
