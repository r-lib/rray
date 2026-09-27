#' Logical operations
#'
#' @description
#' - `rray_and()` tests whether both `x` and `y` are `TRUE`.
#'
#' - `rray_or()` tests whether at least one of `x` and `y` is `TRUE`.
#'
#' - `rray_xor()` tests whether exactly one of `x` and `y` is `TRUE`.
#'
#' @details
#' The arrays are broadcast to common dimensions before the operation is
#' applied.
#'
#' Missing values are handled the same way as `&`, `|`, and [xor()].
#'
#' @param x A logical array.
#'
#' @param y A logical array.
#'
#' @returns
#' A logical array with the common dimensions of `x` and `y`.
#'
#' @name rray-logical
#' @examples
#' x <- array(c(TRUE, FALSE, TRUE), c(3L, 1L))
#' y <- array(c(TRUE, FALSE), c(1L, 2L))
#'
#' rray_and(x, y)
#' rray_or(x, y)
#' rray_xor(x, y)
#'
#' rray_and(NA, FALSE)
#' rray_or(NA, TRUE)
#' rray_xor(NA, TRUE)
NULL

#' @rdname rray-logical
#' @export
rray_and <- function(x, y) {
  .Call(ffi_rray_and, x, y, environment())
}

#' @rdname rray-logical
#' @export
rray_or <- function(x, y) {
  .Call(ffi_rray_or, x, y, environment())
}

#' @rdname rray-logical
#' @export
rray_xor <- function(x, y) {
  .Call(ffi_rray_xor, x, y, environment())
}
