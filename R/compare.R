#' Compare arrays
#'
#' @description
#' - `rray_equal()` tests whether `x` is equal to `y`.
#'
#' - `rray_not_equal()` tests whether `x` is not equal to `y`.
#'
#' - `rray_greater_than()` tests whether `x` is greater than `y`.
#'
#' - `rray_greater_than_or_equal()` tests whether `x` is greater than or equal
#'   to `y`.
#'
#' - `rray_less_than()` tests whether `x` is less than `y`.
#'
#' - `rray_less_than_or_equal()` tests whether `x` is less than or equal to
#'   `y`.
#'
#' @details
#' The arrays are broadcast to common dimensions before they are compared.
#' Missing values, including `NaN`, produce a missing value in the output.
#' Logical, integer, and double arrays are supported.
#'
#' @param x An array.
#'
#' @param y An array.
#'
#' @returns
#' A logical array with the common dimensions of `x` and `y`.
#'
#' @name compare
#' @examples
#' x <- array(1:6, c(3L, 2L))
#'
#' rray_greater_than(x, 3L)
#' rray_less_than_or_equal(x, array(c(2L, 5L), c(1L, 2L)))
NULL

#' @rdname compare
#' @export
rray_equal <- function(x, y) {
  .Call(ffi_rray_equal, x, y, environment())
}

#' @rdname compare
#' @export
rray_not_equal <- function(x, y) {
  .Call(ffi_rray_not_equal, x, y, environment())
}

#' @rdname compare
#' @export
rray_greater_than <- function(x, y) {
  .Call(ffi_rray_greater_than, x, y, environment())
}

#' @rdname compare
#' @export
rray_greater_than_or_equal <- function(x, y) {
  .Call(ffi_rray_greater_than_or_equal, x, y, environment())
}

#' @rdname compare
#' @export
rray_less_than <- function(x, y) {
  .Call(ffi_rray_less_than, x, y, environment())
}

#' @rdname compare
#' @export
rray_less_than_or_equal <- function(x, y) {
  .Call(ffi_rray_less_than_or_equal, x, y, environment())
}
