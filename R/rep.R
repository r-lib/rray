#' Repeat an array
#'
#' @description
#' - `rray_rep()` repeats the whole `axis` of `x`, `times` times. The dimension
#'   along `axis` becomes `dimension * times`.
#'
#' - `rray_rep_each()` repeats each slice of `axis` in place. The dimension
#'   along `axis` becomes `sum(times)`.
#'
#' These are the array versions of [vctrs::vec_rep()] and
#' [vctrs::vec_rep_each()], which repeat along the size of a vector rather than
#' along an axis.
#'
#' @details
#' Names repeat alongside the data, so the repeated axis can come back with
#' duplicate names. Every other axis keeps its names untouched.
#'
#' The type of `x` is always preserved.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x An array.
#'
#' @param times For `rray_rep()`, a single whole number greater than or equal
#'   to 0.
#'
#'   For `rray_rep_each()`, a vector of whole numbers greater than or equal to
#'   0. It is recycled to the dimension of `x` along `axis`.
#'
#' @param axis A single integer representing the axis to repeat along.
#'
#' @returns
#' An array with the same dimensions as `x`, except along `axis`.
#'
#' @name rray-rep
#' @examples
#' x <- array(1:6, c(3, 2))
#'
#' # The whole axis, twice
#' rray_rep(x, times = 2, axis = 1)
#'
#' # Each row, twice
#' rray_rep_each(x, times = 2, axis = 1)
#'
#' # Columns rather than rows
#' rray_rep(x, times = 2, axis = 2)
#'
#' # A different number of repeats per row
#' rray_rep_each(x, times = c(1, 2, 3), axis = 1)
#'
#' # Zeroes drop slices entirely
#' rray_rep_each(x, times = c(1, 0, 2), axis = 1)
#'
#' # Names repeat with the data, duplicates and all
#' y <- array(1:6, c(3, 2), dimnames = list(c("a", "b", "c"), c("x", "y")))
#' rray_names(rray_rep(y, times = 2, axis = 1))
#' rray_names(rray_rep_each(y, times = 2, axis = 1))
NULL

#' @rdname rray-rep
#' @export
rray_rep <- function(x, ..., times, axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_rep, x, times, axis, environment())
}

#' @rdname rray-rep
#' @export
rray_rep_each <- function(x, ..., times, axis) {
  check_dots_empty0(...)
  .Call(ffi_rray_rep_each, x, times, axis, environment())
}
