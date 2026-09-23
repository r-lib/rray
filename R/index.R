#' Index an array with coordinate arrays
#'
#' @description
#' `rray_index()` uses one integer coordinate array for each axis of `x`. The
#' coordinate arrays are broadcast to common dimensions, then read pointwise.
#' The common dimensions become the dimensions of the result.
#'
#' `rray_as_index_array()` validates and normalizes one coordinate array. Each
#' non-missing coordinate must be between `1L` and `dimension`. A vector is
#' normalized to a one-dimensional array.
#'
#' @param x For `rray_index()`, an array to index. For
#'   `rray_as_index_array()`, an integer coordinate array.
#'
#' @param ... One unnamed integer coordinate array for each axis of `x`.
#'
#' @param dimension A single non-negative integer giving the dimension of the
#'   source axis.
#'
#' @returns
#' `rray_index()` returns an array with the common dimensions of `...` and the
#' same storage type as `x`.
#'
#' `rray_as_index_array()` returns `x` as a validated integer array.
#'
#' @export
#' @examples
#' x <- array(1:6, c(2L, 3L))
#'
#' rows <- array(c(1L, 2L), c(2L, 1L))
#' columns <- array(1:3, c(1L, 3L))
#' rray_index(x, rows, columns)
rray_index <- function(x, ...) {
  .Call(ffi_rray_index, x, list2(...), environment())
}

#' @rdname rray_index
#' @export
rray_as_index_array <- function(x, dimension) {
  .Call(ffi_rray_as_index_array, x, dimension, environment())
}
