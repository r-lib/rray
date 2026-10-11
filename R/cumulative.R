#' Accumulate along an axis
#'
#' @description
#' - `rray_cumulative_all()` checks if all values are `TRUE` so far along
#'   `axis`.
#'
#' - `rray_cumulative_any()` checks if any value is `TRUE` so far along
#'   `axis`.
#'
#' - `rray_cumulative_max()` computes the maximum so far along `axis`.
#'
#' - `rray_cumulative_min()` computes the minimum so far along `axis`.
#'
#' @details
#' Each position along `axis` summarizes every position up to and including
#' it.
#'
#' @section All / Any:
#' `x` must be logical. Missing values follow [all()] and [any()], so a later
#' `FALSE` or `TRUE` can settle an earlier `NA`:
#'
#' ```r
#' rray_cumulative_all(c(TRUE, NA, FALSE), 1L) # TRUE, NA, FALSE
#' rray_cumulative_any(c(FALSE, NA, TRUE), 1L) # FALSE, NA, TRUE
#' ```
#'
#' @section Min / Max:
#' `x` must be logical, integer, or double, and the result keeps the type of
#' `x`. Once a line hits `NA` or `NaN`, the rest of it stays missing. A later
#' `NA` replaces an earlier `NaN`, like [max()] and [min()]:
#'
#' ```r
#' rray_cumulative_max(c(1, NaN, 2, NA, 3), 1L) # 1, NaN, NaN, NA, NA
#' ```
#'
#' For logical `x`, a later `TRUE` or `FALSE` does not settle an earlier `NA`,
#' unlike `rray_cumulative_any()` and `rray_cumulative_all()`:
#'
#' ```r
#' rray_cumulative_max(c(NA, TRUE), 1L) # NA, NA
#' rray_cumulative_any(c(NA, TRUE), 1L) # NA, TRUE
#' ```
#'
#' @param x An array.
#'
#' @param axis A single integer. The axis to accumulate along. `1`
#'   accumulates down rows, `2` accumulates across columns, and so on.
#'
#' @returns
#' An array with the same dimensions and names as `x`.
#'
#' @name rray-cumulative
#' @examples
#' x <- array(c(TRUE, TRUE, FALSE, TRUE, NA, TRUE), c(2L, 3L))
#' x
#'
#' # Down rows
#' rray_cumulative_all(x, 1L)
#' rray_cumulative_any(x, 1L)
#'
#' # Across columns
#' rray_cumulative_all(x, 2L)
#' rray_cumulative_any(x, 2L)
#'
#' x <- array(c(3L, 1L, 2L, 5L, 4L, 0L), c(2L, 3L))
#' x
#'
#' # Running maximum and minimum across columns
#' rray_cumulative_max(x, 2L)
#' rray_cumulative_min(x, 2L)
NULL

#' @rdname rray-cumulative
#' @export
rray_cumulative_all <- function(x, axis) {
  .Call(ffi_rray_cumulative_all, x, axis, environment())
}

#' @rdname rray-cumulative
#' @export
rray_cumulative_any <- function(x, axis) {
  .Call(ffi_rray_cumulative_any, x, axis, environment())
}

#' @rdname rray-cumulative
#' @export
rray_cumulative_max <- function(x, axis) {
  .Call(ffi_rray_cumulative_max, x, axis, environment())
}

#' @rdname rray-cumulative
#' @export
rray_cumulative_min <- function(x, axis) {
  .Call(ffi_rray_cumulative_min, x, axis, environment())
}
