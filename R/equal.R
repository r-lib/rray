#' Test arrays for equality
#'
#' @description
#' - `rray_equal()` tests whether `x` is equal to `y`.
#'
#' - `rray_not_equal()` tests whether `x` is not equal to `y`.
#'
#' @details
#' The arrays are broadcast to common dimensions before they are compared.
#'
#' @param x An array.
#'
#' @param y An array.
#'
#' @returns
#' A logical array with the common dimensions of `x` and `y`.
#'
#' @name equal
#' @examples
#' x <- array(c(1 + 1i, 2 + 2i), c(2L, 1L))
#'
#' rray_equal(x, 1 + 1i)
#' rray_not_equal(x, array(c(1 + 1i, 3 + 3i), c(1L, 2L)))
NULL

#' @rdname equal
#' @export
rray_equal <- function(x, y) {
  .Call(ffi_rray_equal, x, y, environment())
}

#' @rdname equal
#' @export
rray_not_equal <- function(x, y) {
  .Call(ffi_rray_not_equal, x, y, environment())
}
