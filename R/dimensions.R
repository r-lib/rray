#' Get the dimensions of an array
#'
#' `rray_dimensions()` returns the dimension of each axis of an
#' array.
#'
#' @details
#' For a plain vector without a `dim` attribute, this returns its
#' length as a single integer (i.e., a 1-dimensional result).
#'
#' @param x An array.
#'
#' @returns
#' An integer vector of dimensions.
#'
#' @export
#' @examples
#' rray_dimensions(1:5)
#' rray_dimensions(array(1, c(2, 3)))
#' rray_dimensions(array(1, c(2, 3, 4)))
rray_dimensions <- function(x) {
  .Call(ffi_rray_dimensions, x, environment())
}

#' Find common dimensions
#'
#' @description
#' `rray_dimensions_common()` finds the common dimensions among multiple
#' arrays using broadcasting rules. For each axis, dimensions are compatible
#' if they are equal or if one of them is 1.
#'
#' @param ... Arrays. `NULL` inputs are silently dropped.
#'
#' @param .dimensions If provided, an integer vector of dimensions to use
#'   as an override, rather than computing common dimensions from `...`.
#'
#' @returns
#' An integer vector of common dimensions.
#'
#' @export
#' @examples
#' rray_dimensions_common(array(1, c(2, 3)), array(1, c(1, 3)))
#' rray_dimensions_common(1:5, array(1, c(1, 3)))
#' rray_dimensions_common(1:5, .dimensions = c(5L, 3L))
rray_dimensions_common <- function(..., .dimensions = NULL) {
  .Call(ffi_rray_dimensions_common, list2(...), .dimensions, environment())
}

#' Set the dimensions of an array
#'
#' @description
#' `rray_set_dimensions()` sets the dimensions of `x` to a new set of dimensions
#' without changing the total number of elements. Unlike [rray_broadcast()],
#' which repeats elements to fill new dimensions, `rray_set_dimensions()` simply
#' reinterprets the existing elements under new dimensions without changing its
#' size.
#'
#' @param x An array.
#'
#' @param dimensions An integer vector of new dimensions.
#'
#' @returns
#' An array with new `dimensions` but the same size as `x`.
#'
#' @export
#' @examples
#' x <- 1:6
#'
#' # Set the dimensions to turn a vector into a matrix
#' rray_set_dimensions(x, c(2L, 3L))
#'
#' # Set the dimensions to turn a vector into a 3D array
#' rray_set_dimensions(x, c(3L, 2L, 1L))
#'
#' # Setting dimensions can't change the size
#' try(rray_set_dimensions(x, c(6L, 2L)))
rray_set_dimensions <- function(x, dimensions) {
  .Call(ffi_rray_set_dimensions, x, dimensions, environment())
}
