#' Accumulate along an axis
#'
#' @description
#' - `rray_cumulative_all()` checks if all values are `TRUE` so far along
#'   `axis`.
#'
#' - `rray_cumulative_any()` checks if any value is `TRUE` so far along
#'   `axis`.
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
