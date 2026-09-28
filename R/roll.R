#' Roll an array
#'
#' @description
#' `rray_roll()` shifts the elements of `x` along one or more axes, with one
#' shift per axis. An element pushed off one end comes back on the other.
#'
#' @details
#' A positive shift moves elements toward the end of the axis, and a negative
#' shift moves them toward the start. Shifts wrap around, so a shift of 7 along
#' an axis with dimension 5 is the same as a shift of 2. A shift of 0, or any
#' multiple of the dimension, leaves the axis unchanged.
#'
#' Names on a rolled axis move with the data. Every other axis keeps its names
#' untouched.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x An array.
#'
#' @param n An integer vector of shifts, either size 1 or the size of `axes`.
#'   A single shift is used for every axis in `axes`. Otherwise `n[[i]]` is the
#'   shift for `axes[[i]]`.
#'
#' @param axes An integer vector of axes to roll along, in any order. Each axis
#'   can appear at most once.
#'
#' @returns
#' An array with the same type and dimensions as `x`.
#'
#' @name rray-roll
#' @examples
#' # Positive shifts move elements toward the end
#' rray_roll(1:5, n = 2, axes = 1)
#'
#' # Negative shifts move them toward the start
#' rray_roll(1:5, n = -1, axes = 1)
#'
#' # Shifts wrap around
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
#' # Roll both axes by the same shift
#' rray_roll(x, n = 1, axes = c(1, 2))
#'
#' # Or by one shift per axis
#' rray_roll(x, n = c(1, -1), axes = c(1, 2))
NULL

#' @rdname rray-roll
#' @export
rray_roll <- function(x, ..., n, axes) {
  check_dots_empty0(...)
  .Call(ffi_rray_roll, x, n, axes, environment())
}
