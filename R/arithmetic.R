#' Array arithmetic
#'
#' @description
#' - `rray_add()` adds two arrays elementwise.
#'
#' - `rray_subtract()` subtracts two arrays elementwise.
#'
#' - `rray_multiply()` multiplies two arrays elementwise.
#'
#' - `rray_divide()` divides two arrays elementwise.
#'
#' - `rray_exponentiate()` raises the elements of one array to the power of
#'   the elements of another.
#'
#' @details
#' The arrays are broadcast to common dimensions first, so they do not have to
#' be the same shape.
#'
#' If the result of an integer operation would overflow, an error is thrown.
#'
#' @section Casting:
#' Certain inputs are upcast, changing the return type:
#'
#' - `rray_add()`: logicals are cast to integer.
#'
#' - `rray_subtract()`: logicals are cast to integer.
#'
#' - `rray_multiply()`: logicals are cast to integer.
#'
#' - `rray_divide()`: logicals and integers are cast to double.
#'
#' - `rray_exponentiate()`: logicals and integers are cast to double.
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
#' rray_subtract(x, 1L)
#' rray_multiply(x, 2L)
#' rray_divide(x, 2L)
#' rray_exponentiate(x, 2L)
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
rray_subtract <- function(x, y) {
  .Call(ffi_rray_subtract, x, y, environment())
}

#' @rdname arithmetic
#' @export
rray_multiply <- function(x, y) {
  .Call(ffi_rray_multiply, x, y, environment())
}

#' @rdname arithmetic
#' @export
rray_divide <- function(x, y) {
  .Call(ffi_rray_divide, x, y, environment())
}

#' @rdname arithmetic
#' @export
rray_exponentiate <- function(x, y) {
  .Call(ffi_rray_exponentiate, x, y, environment())
}
