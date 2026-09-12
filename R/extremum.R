#' Parallel maxima and minima
#'
#' @description
#' - `rray_max()` computes the elementwise maximum of two arrays.
#'
#' - `rray_min()` computes the elementwise minimum of two arrays.
#'
#' @details
#' The arrays are broadcast to common dimensions first, so they do not have to
#' be the same shape.
#'
#' @param x An array.
#'
#' @param y An array.
#'
#' @param ... These dots are for future extensions and must be empty.
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
#' rray_max(x, 4L)
#' rray_min(x, 4L)
#'
#' # Compare each column with a different value through broadcasting
#' rray_max(x, array(c(2L, 5L), c(1L, 2L)))
NULL

#' @rdname extremum
#' @export
rray_max <- function(x, y, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_max, x, y, na_rm, environment())
}

#' @rdname extremum
#' @export
rray_min <- function(x, y, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_min, x, y, na_rm, environment())
}
