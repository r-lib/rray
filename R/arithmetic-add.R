#' Add two arrays
#'
#' @description
#' `rray_add()` adds two arrays elementwise. The arrays are broadcast to
#' common dimensions first, so they do not have to be the same shape.
#'
#' @details
#' The output type is the common type of `x` and `y`, promoted through the
#' rules for `+`. Logical and integer arrays add to an integer array, double
#' arrays to a double array, and complex arrays to a complex array. Character,
#' raw, and list arrays are an error.
#'
#' If adding two integer arrays would overflow, an error is thrown.
#'
#' @param x An array.
#'
#' @param y An array.
#'
#' @returns
#' An array with the common dimensions of `x` and `y`.
#'
#' @export
#' @examples
#' x <- array(1:6, c(3L, 2L))
#'
#' rray_add(x, 1L)
#'
#' # Adding 10 to column 1 and 20 to column 2 via broadcasting
#' rray_add(x, array(c(10L, 20L), c(1L, 2L)))
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
rray_add <- function(x, y) {
  .Call(ffi_rray_add, x, y, environment())
}
