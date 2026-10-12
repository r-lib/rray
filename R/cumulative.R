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
#' - `rray_cumulative_prod()` computes the product so far along `axis`.
#'
#' - `rray_cumulative_sum()` computes the sum so far along `axis`.
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
#' rray_cumulative_all(c(TRUE, NA, FALSE), axis = 1L) # TRUE, NA, FALSE
#' rray_cumulative_any(c(FALSE, NA, TRUE), axis = 1L) # FALSE, NA, TRUE
#' ```
#'
#' @section Min / Max:
#' `x` must be logical, integer, or double, and the result keeps the type of
#' `x`. Once a line hits `NA` or `NaN`, the rest of it stays missing. A later
#' `NA` replaces an earlier `NaN`, like [max()] and [min()]:
#'
#' ```r
#' rray_cumulative_max(c(1, NaN, 2, NA, 3), axis = 1L) # 1, NaN, NaN, NA, NA
#' ```
#'
#' For logical `x`, a later `TRUE` or `FALSE` does not settle an earlier `NA`,
#' unlike `rray_cumulative_any()` and `rray_cumulative_all()`:
#'
#' ```r
#' rray_cumulative_max(c(NA, TRUE), axis = 1L) # NA, NA
#' rray_cumulative_any(c(NA, TRUE), axis = 1L) # NA, TRUE
#' ```
#'
#' @section Product:
#' `x` must be logical, integer, double, or complex. Logicals and integers are
#' cast to double.
#'
#' When `NA` and `NaN` are both present, you get one of them, but which one
#' depends on the platform and the order of the values. Either way, `is.na()`
#' is `TRUE` from that point on:
#'
#' ```r
#' rray_cumulative_prod(c(2, NA, NaN), 1L) # 2, NA, NA or NaN
#' ```
#'
#' @section Sum:
#' `x` must be logical, integer, double, or complex. Logicals are cast to
#' integer.
#'
#' An integer sum errors as soon as a running total overflows, even if a
#' later value would bring it back in range. This differs from [rray_sum()],
#' which only checks the final total:
#'
#' ```r
#' x <- c(.Machine$integer.max, 1L, -1L)
#' rray_sum(x, axes = 1L) # .Machine$integer.max
#' rray_cumulative_sum(x, 1L) # Error
#' ```
#'
#' Once a line is `NA`, it stays `NA`, and a later overflow does not error.
#'
#' When `NA` and `NaN` are both present, you get one of them, but which one
#' depends on the platform and the order of the values. Either way, `is.na()`
#' is `TRUE` from that point on:
#'
#' ```r
#' rray_cumulative_sum(c(2, NA, NaN), 1L) # 2, NA, NA or NaN
#' ```
#'
#' @inheritParams rlang::args_dots_empty
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
#' rray_cumulative_all(x, axis = 1L)
#' rray_cumulative_any(x, axis = 1L)
#'
#' # Across columns
#' rray_cumulative_all(x, axis = 2L)
#' rray_cumulative_any(x, axis = 2L)
#'
#' x <- array(c(3L, 1L, 2L, 5L, 4L, 0L), c(2L, 3L))
#' x
#'
#' # Running maximum and minimum across columns
#' rray_cumulative_max(x, axis = 2L)
#' rray_cumulative_min(x, axis = 2L)
#'
#' # Running product down rows
#' rray_cumulative_prod(x, 1L)
#'
#' # Running sum across columns
#' rray_cumulative_sum(x, 2L)
NULL

#' @rdname rray-cumulative
#' @export
rray_cumulative_all <- function(x, ..., axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_cumulative_all, x, axis, environment())
}

#' @rdname rray-cumulative
#' @export
rray_cumulative_any <- function(x, ..., axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_cumulative_any, x, axis, environment())
}

#' @rdname rray-cumulative
#' @export
rray_cumulative_max <- function(x, ..., axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_cumulative_max, x, axis, environment())
}

#' @rdname rray-cumulative
#' @export
rray_cumulative_min <- function(x, ..., axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_cumulative_min, x, axis, environment())
}

#' @rdname rray-cumulative
#' @export
rray_cumulative_prod <- function(x, axis) {
  .Call(ffi_rray_cumulative_prod, x, axis, environment())
}

#' @rdname rray-cumulative
#' @export
rray_cumulative_sum <- function(x, axis) {
  .Call(ffi_rray_cumulative_sum, x, axis, environment())
}
