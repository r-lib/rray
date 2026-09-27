#' Slice an array
#'
#' @description
#' `rray_slice()` selects positions along each axis of `x`. The result always
#' has the same dimensionality as `x`.
#'
#' `rray_slice_assign()` replaces the selected positions with `value` and
#' returns a modified copy of `x`.
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
#' @param x An array.
#'
#' @param ... One unnamed subscript for each axis of `x`, in axis order. Each
#'   subscript is one of:
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
#' @param value An array to assign to the selected positions. It is cast to
#'   the type of `x`, then broadcast to the dimensions of the selection, which
#'   are the dimensions of `rray_slice(x, ...)`.
#'
#' @returns
#' - `rray_slice()` returns an array with the same type and dimensionality as
#'   `x`. The dimension of each axis is the number of positions selected on
#'   it. The names of each axis are selected along with the values.
#'
#' - `rray_slice_assign()` returns `x` with the selected positions replaced.
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
#' # Assign one value to every selected position
#' rray_slice_assign(x, 1, TRUE, TRUE, value = 0L)
#'
#' # Or broadcast `value` to the dimensions of the selection
#' value <- array(c(100L, 200L), c(1L, 2L))
#' rray_slice_assign(x, TRUE, c(1, 3), 1, value = value)
rray_slice <- function(x, ...) {
  .Call(ffi_rray_slice, x, list2(...), environment())
}

#' @rdname rray_slice
#' @export
rray_slice_assign <- function(x, ..., value) {
  .Call(ffi_rray_slice_assign, x, list2(...), value, environment())
}
