#' Slice an array
#'
#' @description
#' `rray_slice()` selects positions along each axis of `x`. The result always
#' has the same dimensionality as `x`.
#'
#' `rray_slice_axis()` selects positions along a single `axis` of `x`. Every
#' other axis is kept whole. `rray_slice_rows()` and `rray_slice_columns()` are
#' shortcuts for `axis = 1` and `axis = 2`.
#'
#' The `_assign` versions replace the selected positions with `value` and
#' return a modified copy of `x`.
#'
#' @details
#' `NA` in a subscript gives missing values in the result. For raw arrays the
#' missing value is `as.raw(0)`, and for list arrays it is `NULL`. If the axis
#' has names, the name of a missing value is `""`. For assignment, `NA` in a
#' subscript leaves `x` unchanged at that position but uses one element of
#' `value`.
#'
#' Unlike `[`, an empty argument does not select a whole axis. Use `TRUE`
#' instead, so that every axis is provided explicitly:
#'
#' ```r
#' x <- array(1:24, c(2L, 3L, 4L))
#'
#' # Base R
#' x[1, , , drop = FALSE]
#'
#' # rray
#' rray_slice(x, 1, TRUE, TRUE)
#' ```
#'
#' When the axis is held in a variable, use `rray_slice_axis()` rather than
#' padding a call to `rray_slice()` with `TRUE`:
#'
#' ```r
#' # These are the same
#' rray_slice(x, TRUE, TRUE, c(4, 1))
#' rray_slice_axis(x, c(4, 1), axis = 3)
#' ```
#'
#' @param x An array.
#'
#' @param ... For `rray_slice()` and `rray_slice_assign()`, one unnamed
#'   subscript for each axis of `x`, in axis order. Each subscript is one of:
#'
#'   - `TRUE`, to select the whole axis.
#'
#'   - A logical vector the size of the axis. `TRUE` selects a position.
#'
#'   - An integer or double vector of locations. Negative values drop
#'     positions, zero is ignored, and duplicates repeat positions.
#'
#'   - A character vector of names, matched against the names of the axis. The
#'     first match is used when names are duplicated.
#'
#'   - `NULL`, to select nothing.
#'
#'   For `rray_slice_axis()` and `rray_slice_assign_axis()`, these dots must be
#'   empty.
#'
#' @param i A subscript for the selected axis. It takes any of the forms
#'   allowed in `...`.
#'
#' @param axis A single integer. The axis to slice along.
#'
#' @param value An array to assign to the selected positions. It is cast to
#'   the type of `x`, then broadcast to the dimensions of the selection. Those
#'   are the dimensions of the matching read, like `rray_slice(x, ...)` for
#'   `rray_slice_assign()`.
#'
#' @returns
#' - `rray_slice()`, `rray_slice_axis()`, `rray_slice_rows()`, and
#'   `rray_slice_columns()` return an array with the same type and
#'   dimensionality as `x`. The dimension of each axis is the number of
#'   positions selected on it. The names of each axis are selected along with
#'   the values.
#'
#' - The `_assign` versions return `x` with the selected positions replaced.
#'   The type, dimensions, and names of `x` are kept.
#'
#' @export
#' @examples
#' x <- array(1:24, c(2L, 3L, 4L))
#'
#' # The first row
#' rray_slice(x, 1, TRUE, TRUE)
#'
#' # Reorder and repeat positions
#' rray_slice(x, c(2, 1, 2), 1, 4)
#'
#' # Drop positions with negative locations
#' rray_slice(x, TRUE, -2, -(1:2))
#'
#' # Select by name
#' y <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("c", "d", "e")))
#' rray_slice(y, "b", c("e", "c"))
#'
#' # Subscripts can be spliced into `...`
#' subscripts <- rep(list(TRUE), rray_dimensionality(x))
#' subscripts[[3]] <- c(4, 1)
#' rray_slice(x, !!!subscripts)
#'
#' # Slice a single axis
#' rray_slice_axis(x, c(4, 1), axis = 3)
#' rray_slice_rows(y, c("b", "a"))
#' rray_slice_columns(x, -2)
#'
#' # Assign one value to every first row
#' rray_slice_assign(x, 1, TRUE, TRUE, value = 0L)
#' rray_slice_assign_rows(x, 1, 0L)
#'
#' # Or broadcast `value` to the dimensions of the selection
#' value <- array(c(100L, 200L), c(1L, 2L))
#' rray_slice_assign(x, TRUE, c(1, 3), 1, value = value)
#'
#' value <- array(c(100L, 200L), c(2L, 1L))
#' rray_slice_assign_axis(x, 3, axis = 2, value = value)
rray_slice <- function(x, ...) {
  .Call(ffi_rray_slice, x, list2(...), environment())
}

#' @rdname rray_slice
#' @export
rray_slice_assign <- function(x, ..., value) {
  .Call(ffi_rray_slice_assign, x, list2(...), value, environment())
}

#' @rdname rray_slice
#' @export
rray_slice_axis <- function(x, i, ..., axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_slice_axis, x, i, axis, environment())
}

#' @rdname rray_slice
#' @export
rray_slice_rows <- function(x, i) {
  .Call(ffi_rray_slice_rows, x, i, environment())
}

#' @rdname rray_slice
#' @export
rray_slice_columns <- function(x, i) {
  .Call(ffi_rray_slice_columns, x, i, environment())
}

#' @rdname rray_slice
#' @export
rray_slice_assign_axis <- function(x, i, ..., axis, value) {
  check_dots_empty0(...)
  .Call(ffi_rray_slice_assign_axis, x, i, axis, value, environment())
}

#' @rdname rray_slice
#' @export
rray_slice_assign_rows <- function(x, i, value) {
  .Call(ffi_rray_slice_assign_rows, x, i, value, environment())
}

#' @rdname rray_slice
#' @export
rray_slice_assign_columns <- function(x, i, value) {
  .Call(ffi_rray_slice_assign_columns, x, i, value, environment())
}
