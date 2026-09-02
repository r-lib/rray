#' Get names for each axis of an array
#'
#' `rray_names()` returns the names for each axis of an array, or `NULL` if `x`
#' has no names at all. Unlike [dimnames()], it returns a one-element list for
#' named vectors.
#'
#' @param x An array.
#'
#' @returns
#' Either:
#' - A list with length equal to the dimensionality of `x`, where each element
#'   is either a character vector of names or `NULL`.
#' - `NULL` if `x` has no names.
#'
#' @export
#' @examples
#' # No names at all
#' rray_names(1:3)
#' # Named vectors return a one-element list
#' rray_names(c(a = 1, b = 2))
#'
#' # All dimension names
#' x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
#' rray_names(x)
rray_names <- function(x) {
  .Call(ffi_rray_names, x, environment())
}

#' Get names for a single axis of an array
#'
#' @description
#' - `rray_axis_names()` returns the names for a single `axis` of an array,
#'   or `NULL` if that axis has no names.
#'
#' - `rray_row_names()` and `rray_col_names()` are shortcuts for
#'   `rray_axis_names(x, 1)` and `rray_axis_names(x, 2)`.
#'
#' @param x An array.
#'
#' @param axis A single integer. The axis to get names for.
#'
#' @returns
#' A character vector of names, or `NULL` if that axis has no names.
#'
#' @export
#' @examples
#' x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
#'
#' rray_axis_names(x, 1)
#' rray_axis_names(x, 2)
#'
#' rray_row_names(x)
#' rray_col_names(x)
rray_axis_names <- function(x, axis) {
  .Call(ffi_rray_axis_names, x, axis, environment())
}

#' @rdname rray_axis_names
#' @export
rray_row_names <- function(x) {
  .Call(ffi_rray_row_names, x, environment())
}

#' @rdname rray_axis_names
#' @export
rray_col_names <- function(x) {
  .Call(ffi_rray_col_names, x, environment())
}

#' Set names for every axis of an array
#'
#' `rray_set_names()` sets the names for every axis of an array at once.
#'
#' @param x An array.
#'
#' @param names A list with length equal to the dimensionality of `x`, where
#'   each element is either a character vector of names for that axis or
#'   `NULL`. Can also be `NULL` to remove all names from `x`.
#'
#' @returns
#' `x` with new names.
#'
#' @export
#' @examples
#' x <- array(1:6, c(2, 3))
#'
#' rray_set_names(x, list(c("r1", "r2"), c("c1", "c2", "c3")))
#'
#' # `NULL` clears all names
#' y <- rray_set_names(x, list(c("r1", "r2"), NULL))
#' rray_set_names(y, NULL)
rray_set_names <- function(x, names) {
  .Call(ffi_rray_set_names, x, names, environment())
}

#' Set names for a single axis of an array
#'
#' @description
#' - `rray_set_axis_names()` sets the names for a single `axis` of an array,
#'   leaving every other axis untouched.
#'
#' - `rray_set_row_names()` and `rray_set_col_names()` are shortcuts for
#'   `rray_set_axis_names(x, 1, names)` and `rray_set_axis_names(x, 2, names)`.
#'
#' @param x An array.
#'
#' @param axis A single integer. The axis to set names for.
#'
#' @param names A character vector of names for `axis`, the same length as
#'   the dimension of `axis`. Can also be `NULL` to remove names from `axis`.
#'
#' @returns
#' `x` with new names for `axis`.
#'
#' @export
#' @examples
#' x <- array(1:6, c(2, 3))
#'
#' rray_set_axis_names(x, 1, c("r1", "r2"))
#' rray_set_axis_names(x, 2, c("c1", "c2", "c3"))
#'
#' rray_set_row_names(x, c("r1", "r2"))
#' rray_set_col_names(x, c("c1", "c2", "c3"))
rray_set_axis_names <- function(x, axis, names) {
  .Call(ffi_rray_set_axis_names, x, axis, names, environment())
}

#' @rdname rray_set_axis_names
#' @export
rray_set_row_names <- function(x, names) {
  .Call(ffi_rray_set_row_names, x, names, environment())
}

#' @rdname rray_set_axis_names
#' @export
rray_set_col_names <- function(x, names) {
  .Call(ffi_rray_set_col_names, x, names, environment())
}
