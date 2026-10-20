#' Elementwise maximum and minimum
#'
#' @description
#' - `rray_pmax()` computes the elementwise maximum of two arrays.
#'
#' - `rray_pmin()` computes the elementwise minimum of two arrays.
#'
#' @details
#' The arrays are broadcast to common dimensions first, so they do not have to
#' be the same shape.
#'
#' When `NA` and `NaN` are both present, `NA` wins, like [rray_max()],
#' [rray_min()], [max()], and [min()]. This differs from [pmax()] and [pmin()],
#' which return whichever comes last:
#'
#' ```r
#' rray_pmax(NA, NaN) # NA
#' rray_pmax(NaN, NA) # NA
#'
#' pmax(NA, NaN) # NaN
#' ```
#'
#' Names are kept. If `x` and `y` both name an axis, the names must be the
#' same. Names on an axis of dimension 1 that gets broadcast are dropped, so
#' they are never compared. To resolve a conflict, remove the names from one
#' input with [rray_set_names()].
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x An array.
#'
#' @param y An array.
#'
#' @param na_rm If `TRUE`, missing values are removed before taking the maximum
#'   or minimum.
#'
#' @returns
#' An array with the common dimensions and common type of `x` and `y`.
#'
#' @name rray-extremum
#' @examples
#' x <- array(1:6, c(3L, 2L))
#'
#' rray_pmax(x, 4L)
#' rray_pmin(x, 4L)
#'
#' # Compare each column with a different value through broadcasting
#' rray_pmax(x, array(c(2L, 5L), c(1L, 2L)))
#'
#' # Names on a shared axis must match
#' x <- array(1:2, 2L, dimnames = list(c("r1", "r2")))
#' y <- array(3:4, 2L, dimnames = list(c("a", "b")))
#' try(rray_pmax(x, y))
#'
#' # Remove the names from one input to resolve the conflict
#' rray_pmax(x, rray_set_names(y, NULL))
NULL

#' @rdname rray-extremum
#' @export
rray_pmax <- function(x, y, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_pmax, x, y, na_rm, environment())
}

#' @rdname rray-extremum
#' @export
rray_pmin <- function(x, y, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_pmin, x, y, na_rm, environment())
}
