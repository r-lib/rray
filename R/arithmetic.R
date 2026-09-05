#' Array arithmetic
#'
#' @description
#' - `rray_add()` adds two arrays elementwise.
#'
#' - `rray_multiply()` multiplies two arrays elementwise.
#'
#' @details
#' The arrays are broadcast to common dimensions first, so they do not have to
#' be the same shape.
#'
#' If the result of an integer operation would overflow, an error is thrown.
#'
#' @section Casting:
#' `x` and `y` are cast to a common type, then upcast to the output type:
#'
#' - `rray_add()`: logical and integer to integer, double to double, complex
#'   to complex.
#'
#' - `rray_multiply()`: logical and integer to integer, double to double,
#'   complex to complex.
#'
#' Character, raw, and list arrays are an error.
#'
#' @param x An array.
#'
#' @param y An array.
#'
#' @returns
#' An array with the common dimensions of `x` and `y`.
#'
#' @name arithmetic
#' @examples
#' x <- array(1:6, c(3L, 2L))
#'
#' rray_add(x, 1L)
#' rray_multiply(x, 2L)
#'
#' # Adding 10 to column 1 and 20 to column 2 via broadcasting
#' rray_add(x, array(c(10L, 20L), c(1L, 2L)))
#'
#' # Scaling column 1 by 2 and column 2 by 3 the same way
#' rray_multiply(x, array(c(2L, 3L), c(1L, 2L)))
#'
#' # Names are collected from both inputs, one axis at a time
#' rows <- array(1:3, c(3L, 1L), dimnames = list(c("r1", "r2", "r3")))
#' cols <- array(c(10L, 20L), c(1L, 2L), dimnames = list(NULL, c("c1", "c2")))
#' rray_add(rows, cols)
#'
#' # A broadcast axis loses its names, because they no longer describe the
#' # result. `only` names a single row, but the result has three.
#' cols <- array(c(10L, 20L), c(1L, 2L), dimnames = list("only", c("c1", "c2")))
#' rray_add(x, cols)
NULL

#' @rdname arithmetic
#' @export
rray_add <- function(x, y) {
  .Call(ffi_rray_add, x, y, environment())
}

#' @rdname arithmetic
#' @export
rray_multiply <- function(x, y) {
  .Call(ffi_rray_multiply, x, y, environment())
}
