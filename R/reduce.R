#' Reduce an array along axes
#'
#' @description
#' - `rray_sum()` computes the sum along the specified `axes`.
#'
#' - `rray_prod()` computes the product along the specified `axes`.
#'
#' - `rray_mean()` computes the mean along the specified `axes`.
#'
#' - `rray_all()` checks if all values are `TRUE` along the specified `axes`.
#'
#' - `rray_any()` checks if any value is `TRUE` along the specified `axes`.
#'
#' - `rray_max()` computes the maximum along the specified `axes`.
#'
#' - `rray_min()` computes the minimum along the specified `axes`.
#'
#' @details
#' The dimensionality of `x` is retained in the result, with the reduced axes
#' collapsed to size 1.
#'
#' @section Sum:
#' Logicals are summed as integers. If an integer sum doesn't fit in an
#' integer, an error is thrown. Only the final sum is checked, and `NA` wins
#' over an overflow:
#'
#' ```r
#' x <- c(.Machine$integer.max, 1L, -1L)
#' rray_sum(x, 1L) # .Machine$integer.max
#'
#' x <- c(.Machine$integer.max, 1L, NA)
#' rray_sum(x, 1L) # NA
#' ```
#'
#' @section Product:
#' Logicals and integers are cast to double.
#'
#' @section Mean:
#' Logicals and integers are cast to double.
#'
#' When `NA` and `NaN` are both present, you get one of them, but which one
#' depends on the platform and the order of the values, like [mean()]. Either
#' way, `is.na()` is `TRUE`:
#'
#' ```r
#' rray_mean(c(NA, NaN), 1L) # NA or NaN
#' rray_mean(c(NaN, NA), 1L) # NA or NaN
#' ```
#'
#' @section Min / Max:
#' `rray_max()` and `rray_min()` keep the type of `x`. With nothing to reduce,
#' such as an axis of dimension 0, `rray_max()` returns the smallest value of
#' that type and `rray_min()` returns the largest:
#'
#' | type    | `rray_max()`            | `rray_min()`           |
#' |---------|-------------------------|------------------------|
#' | logical | `FALSE`                 | `TRUE`                 |
#' | integer | `-.Machine$integer.max` | `.Machine$integer.max` |
#' | double  | `-Inf`                  | `Inf`                  |
#'
#' When `NA` and `NaN` are both present, `NA` wins, like [max()] and [min()]:
#'
#' ```r
#' rray_max(c(NA, NaN), 1L) # NA
#' rray_max(c(NaN, NA), 1L) # NA
#' ```
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param x An array.
#'
#' @param axes An integer vector of axes to reduce over. `1` reduces
#'   rows, `2` reduces columns, and so on.
#'
#' @param na_rm If `TRUE`, missing values are removed before reducing.
#'
#' @returns
#' An array with the same dimensionality as `x`, but with the dimensions along
#' `axes` reduced to 1.
#'
#' @name rray-reduce
#' @examples
#' x <- array(1:10, c(5L, 2L))
#'
#' # Sum along rows
#' rray_sum(x, 1L)
#'
#' # Sum along columns
#' rray_sum(x, 2L)
#'
#' # Sum along both axes
#' rray_sum(x, c(1L, 2L))
#'
#' # Product along rows
#' rray_prod(x, 1L)
#'
#' # Mean along rows
#' rray_mean(x, 1L)
#'
#' y <- array(c(TRUE, TRUE, FALSE, TRUE), c(2L, 2L))
#'
#' rray_all(y, 1L)
#' rray_any(y, 1L)
#'
#' # Maximum and minimum along columns
#' rray_max(x, 2L)
#' rray_min(x, 2L)
NULL

#' @rdname rray-reduce
#' @export
rray_sum <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_sum, x, axes, na_rm, environment())
}

#' @rdname rray-reduce
#' @export
rray_prod <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_prod, x, axes, na_rm, environment())
}

#' @rdname rray-reduce
#' @export
rray_mean <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_mean, x, axes, na_rm, environment())
}

#' @rdname rray-reduce
#' @export
rray_all <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_all, x, axes, na_rm, environment())
}

#' @rdname rray-reduce
#' @export
rray_any <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_any, x, axes, na_rm, environment())
}

#' @rdname rray-reduce
#' @export
rray_max <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_max, x, axes, na_rm, environment())
}

#' @rdname rray-reduce
#' @export
rray_min <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_min, x, axes, na_rm, environment())
}
