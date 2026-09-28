#' Roll an array
#'
#' @description
#' - `rray_roll()` rolls the elements of `x` along one or more `axes`, with one
#'   roll per axis. Everything along an axis moves by the same amount.
#'
#' - `rray_roll_each()` rolls the elements of `x` along a single `axis`, with
#'   a separate roll for each row, column, and so on. For example, with a
#'   matrix and `axis = 2`, each row can move by a different amount.
#'
#' An element pushed off one end circles back on the other.
#'
#' @details
#' A positive `n` moves elements toward the end of the axis, and a negative `n`
#' moves them toward the start. Rolls circle around, so an `n` of 7 along an
#' axis with dimension 5 is the same as an `n` of 2. An `n` of 0, or any
#' multiple of the dimension, leaves the axis unchanged.
#'
#' For `rray_roll_each()`, `n` is broadcast to the dimensions of `x` with
#' `axis` set to 1. For a 3 x 4 matrix, that is:
#'
#' - `axis = 2` gives 3 x 1, so `n` can be a vector of 3, one per row.
#'
#' - `axis = 1` gives 1 x 4, so `n` can be a 1 x 4 matrix, one per column.
#'
#' A single `n` works in both cases, and rolls everything by the same amount.
#'
#' For `rray_roll()`, names on a rolled axis move with the data. For
#' `rray_roll_each()`, names on `axis` are always dropped, since each row or
#' column can move by a different amount. In both, every other axis keeps its
#' names untouched.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x An array.
#'
#' @param n For `rray_roll()`, an integer vector indicating the amount to roll,
#'   either size 1 or the size of `axes`.
#'
#'   For `rray_roll_each()`, an integer array indicating the amount to roll,
#'   broadcastable to the dimensions of `x` with `axis` set to 1.
#'
#' @param axes An integer vector of axes to roll along.
#'
#' @param axis A single integer representing the axis to roll along.
#'
#' @returns
#' An array with the same type and dimensions as `x`.
#'
#' @name rray-roll
#' @examples
#' # Positive `n` moves elements toward the end
#' rray_roll(1:5, n = 2, axes = 1)
#'
#' # Negative `n` moves them toward the start
#' rray_roll(1:5, n = -1, axes = 1)
#'
#' # Large `n` circles around
#' rray_roll(1:5, n = 7, axes = 1)
#'
#' x <- matrix(1:6, nrow = 2, dimnames = list(c("r1", "r2"), c("a", "b", "c")))
#'
#' # Roll the columns, names move with the data
#' rray_roll(x, n = 1, axes = 2)
#'
#' # Roll the rows
#' rray_roll(x, n = 1, axes = 1)
#'
#' # Roll both axes by the same `n`
#' rray_roll(x, n = 1, axes = c(1, 2))
#'
#' # Or by one `n` per axis
#' rray_roll(x, n = c(1, -1), axes = c(1, 2))
#'
#' y <- matrix(1:12, nrow = 3, byrow = TRUE)
#'
#' # Roll each row by its own `n`
#' rray_roll_each(y, n = c(1, 0, -1), axis = 2)
#'
#' # Roll each column by its own `n`, given as a one row matrix
#' rray_roll_each(y, n = matrix(c(0, 1, 2, 3), nrow = 1), axis = 1)
#'
#' # Names on `axis` are dropped, since no single name fits each position
#' rray_roll_each(x, n = c(1, 2), axis = 2)
NULL

#' @rdname rray-roll
#' @export
rray_roll <- function(x, ..., n, axes) {
  check_dots_empty0(...)
  .Call(ffi_rray_roll, x, n, axes, environment())
}

#' @rdname rray-roll
#' @export
rray_roll_each <- function(x, ..., n, axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_roll_each, x, n, axis, environment())
}
