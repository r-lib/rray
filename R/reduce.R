#' Reduce an array along axes
#'
#' @description
#' - `rray_sum_along()` computes the sum along the specified `axes`.
#'
#' - `rray_product_along()` computes the product along the specified `axes`.
#'
#' - `rray_all_along()` checks if all values are `TRUE` along the specified
#'   `axes`.
#'
#' - `rray_any_along()` checks if any value is `TRUE` along the specified
#'   `axes`.
#'
#' @details
#' The dimensionality of `x` is retained in the result, with the reduced axes
#' collapsed to size 1.
#'
#' If summing an integer array would overflow, an error is thrown.
#'
#' @section Casting:
#' Certain inputs are upcast, changing the return type:
#'
#' - `rray_product_along()`: logicals and integers are cast to double.
#'
#' @param x An array.
#'
#' @param axes An integer vector of axes to reduce over. `1` reduces
#'   rows, `2` reduces columns, and so on.
#'
#' @param ... These dots are for future extensions and must be empty.
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
#' rray_sum_along(x, 1L)
#'
#' # Sum along columns
#' rray_sum_along(x, 2L)
#'
#' # Sum along both axes
#' rray_sum_along(x, c(1L, 2L))
#'
#' # Product along rows
#' rray_product_along(x, 1L)
#'
#' y <- array(c(TRUE, TRUE, FALSE, TRUE), c(2L, 2L))
#'
#' rray_all_along(y, 1L)
#' rray_any_along(y, 1L)
NULL

#' @rdname reduce
#' @export
rray_sum_along <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_sum_along, x, axes, na_rm, environment())
}

#' @rdname reduce
#' @export
rray_product_along <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_product_along, x, axes, na_rm, environment())
}

#' @rdname reduce
#' @export
rray_all_along <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_all_along, x, axes, na_rm, environment())
}

#' @rdname reduce
#' @export
rray_any_along <- function(x, axes, ..., na_rm = FALSE) {
  check_dots_empty0(...)
  .Call(ffi_rray_any_along, x, axes, na_rm, environment())
}
