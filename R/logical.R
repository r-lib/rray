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
#' Names are kept. If `x` and `y` both name an axis, the names must be the
#' same. Names on an axis of dimension 1 that gets broadcast are dropped, so
#' they are never compared. To resolve a conflict, remove the names from one
#' input with [rray_set_names()].
#'
#' @param x,y Logical arrays.
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
#'
#' # Names on a shared axis must match
#' x <- array(c(TRUE, FALSE), 2L, dimnames = list(c("r1", "r2")))
#' y <- array(c(TRUE, TRUE), 2L, dimnames = list(c("a", "b")))
#' try(rray_and(x, y))
#'
#' # Remove the names from one input to resolve the conflict
#' rray_and(x, rray_set_names(y, NULL))
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
