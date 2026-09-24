# TODO!: Rewrite these docs with better descriptions and worked examples when
# back on a computer.

#' Index an array by coordinates
#'
#' `rray_index()` selects values from `x` with one coordinate array for each
#' source axis. The coordinate arrays broadcast to common dimensions and are
#' read pointwise. Their common dimensions become the dimensions of the result.
#'
#' @details
#' Each argument in `...` identifies positions on one axis of `x`. The first
#' argument identifies positions on the first axis, the second argument
#' identifies positions on the second axis, and so on. You must supply exactly
#' one unnamed coordinate array for every axis of `x`.
#'
#' A coordinate array must be a bare integer vector or array. Vectors are
#' treated as one-dimensional arrays. Each coordinate must be a positive,
#' one-based position within its source axis. Zero, negative, and out-of-bounds
#' coordinates are errors.
#'
#' Coordinate arrays use left-aligned broadcasting. At each point in the common
#' dimensions, `rray_index()` takes one coordinate from each broadcast array
#' and uses the complete set of coordinates to select one value from `x`.
#' Singleton dimensions can be placed on different axes to form a Cartesian
#' product.
#'
#' A missing coordinate on any source axis produces a missing value at that
#' result point. For raw arrays the missing value is `as.raw(0)`, and for list
#' arrays it is `NULL`.
#'
#' The result has the same storage type as `x`. Its dimensions are the common
#' dimensions of the coordinate arrays. All names are dropped because a result
#' axis does not necessarily correspond to one source axis.
#'
#' @param x A bare array or vector.
#'
#' @param ... Exactly one unnamed integer coordinate array for each axis of
#'   `x`.
#'
#' @returns
#' An array with the common dimensions of `...` and the same storage type as
#' `x`.
#'
#' @export
#' @examples
#' x <- array(1:6, c(2L, 3L))
#'
#' # Pair coordinates pointwise
#' rray_index(x, c(1L, 2L, 1L), c(1L, 2L, 3L))
#'
#' # Form a Cartesian product through broadcasting
#' rows <- array(c(1L, 2L), c(2L, 1L))
#' columns <- array(1:3, c(1L, 3L))
#' rray_index(x, rows, columns)
#'
#' # Any missing coordinate produces a missing result
#' rray_index(x, c(1L, NA_integer_), c(2L, 3L))
#'
#' # Coordinate lists can be spliced into `...`
#' coordinates <- list(rows, columns)
#' rray_index(x, !!!coordinates)
rray_index <- function(x, ...) {
  .Call(ffi_rray_index, x, list2(...), environment())
}

rray_as_index_array <- function(x, dimension) {
  .Call(ffi_rray_as_index_array, x, dimension, environment())
}
