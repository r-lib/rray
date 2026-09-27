#' Slice an array along one axis
#'
#' @description
#' `rray_slice_axis()` selects positions along a single `axis` of `x`. Every
#' other axis is kept whole. It is the same as calling [rray_slice()] with `i`
#' at position `axis` and `TRUE` everywhere else.
#'
#' `rray_slice_assign_axis()` replaces the selected positions with `value` and
#' returns a modified copy of `x`.
#'
#' `rray_slice_rows()`, `rray_slice_columns()`, and their `_assign` versions
#' are shortcuts for `axis = 1` and `axis = 2`.
#'
#' @details
#' Use `rray_slice_axis()` when the axis is held in a variable, rather than
#' padding a call to [rray_slice()] with `TRUE`:
#'
#' ```r
#' x <- array(1:24, c(2L, 3L, 4L))
#'
#' # These are the same
#' rray_slice(x, TRUE, TRUE, c(4, 1))
#' rray_slice_axis(x, c(4, 1), axis = 3)
#' ```
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x An array.
#'
#' @param i A subscript for `axis`. See the `...` argument of [rray_slice()]
#'   for the kinds of subscript allowed.
#'
#' @param axis A single integer. The axis to slice along.
#'
#' @param value An array to assign to the selected positions. It is cast to
#'   the type of `x`, then broadcast to the dimensions of the selection, which
#'   are the dimensions of `rray_slice_axis(x, i, axis = axis)`.
#'
#' @returns
#' - `rray_slice_axis()` returns an array with the same type and dimensionality
#'   as `x`. Only the dimension of `axis` changes. The names of `axis` are
#'   selected along with the values.
#'
#' - `rray_slice_assign_axis()` returns `x` with the selected positions
#'   replaced. The type, dimensions, and names of `x` are kept.
#'
#' @export
#' @examples
#' x <- array(1:24, c(2L, 3L, 4L))
#'
#' # The last and first positions of the third axis
#' rray_slice_axis(x, c(4, 1), axis = 3)
#'
#' # Drop the second column
#' rray_slice_columns(x, -2)
#'
#' # Select rows by name
#' y <- array(1:6, c(2L, 3L), dimnames = list(c("a", "b"), c("c", "d", "e")))
#' rray_slice_rows(y, c("b", "a"))
#'
#' # Assign one value to every second column
#' rray_slice_assign_columns(x, 2, 0L)
#'
#' # Or broadcast `value` to the dimensions of the selection
#' value <- array(c(100L, 200L), c(2L, 1L))
#' rray_slice_assign_axis(x, 3, axis = 2, value = value)
rray_slice_axis <- function(x, i, ..., axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_slice_axis, x, i, axis, environment())
}

#' @rdname rray_slice_axis
#' @export
rray_slice_rows <- function(x, i) {
  .Call(ffi_rray_slice_rows, x, i, environment())
}

#' @rdname rray_slice_axis
#' @export
rray_slice_columns <- function(x, i) {
  .Call(ffi_rray_slice_columns, x, i, environment())
}

#' @rdname rray_slice_axis
#' @export
rray_slice_assign_axis <- function(x, i, ..., axis, value) {
  check_dots_empty0(...)
  .Call(ffi_rray_slice_assign_axis, x, i, axis, value, environment())
}

#' @rdname rray_slice_axis
#' @export
rray_slice_assign_rows <- function(x, i, value) {
  .Call(ffi_rray_slice_assign_rows, x, i, value, environment())
}

#' @rdname rray_slice_axis
#' @export
rray_slice_assign_columns <- function(x, i, value) {
  .Call(ffi_rray_slice_assign_columns, x, i, value, environment())
}
