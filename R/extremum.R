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
#' @name extremum
#' @examples
#' x <- array(1:6, c(3L, 2L))
#'
#' rray_pmax(x, 4L)
#' rray_pmin(x, 4L)
#'
#' # Compare each column with a different value through broadcasting
#' rray_pmax(x, array(c(2L, 5L), c(1L, 2L)))
NULL

#' @rdname extremum
#' @export
rray_pmax <- function(x, y, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_pmax, x, y, na_rm, environment())
}

#' @rdname extremum
#' @export
rray_pmin <- function(x, y, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_pmin, x, y, na_rm, environment())
}
