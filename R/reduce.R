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
#' If summing an integer array would overflow, an error is thrown.
#'
#' @section Product:
#' Logicals and integers are cast to double.
#'
#' @section Mean:
#' Logicals and integers are cast to double.
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
#' When `NA` and `NaN` are both present, `rray_max()` and `rray_min()` return
#' whichever comes last. This matches [rray_pmax()] and [rray_pmin()], rather
#' than base R's [max()] and [min()], where `NA` always wins:
#'
#' ```r
#' rray_max(c(NA, NaN), 1L) # NaN
#' rray_max(c(NaN, NA), 1L) # NA
#'
#' max(c(NA, NaN)) # NA
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
#' @name reduce
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

#' @rdname reduce
#' @export
rray_sum <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_sum, x, axes, na_rm, environment())
}

#' @rdname reduce
#' @export
rray_prod <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_prod, x, axes, na_rm, environment())
}

#' @rdname reduce
#' @export
rray_mean <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_mean, x, axes, na_rm, environment())
}

#' @rdname reduce
#' @export
rray_all <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_all, x, axes, na_rm, environment())
}

#' @rdname reduce
#' @export
rray_any <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_any, x, axes, na_rm, environment())
}

#' @rdname reduce
#' @export
rray_max <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_max, x, axes, na_rm, environment())
}

#' @rdname reduce
#' @export
rray_min <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_min, x, axes, na_rm, environment())
}
