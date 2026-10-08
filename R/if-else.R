#' Choose values from arrays
#'
#' `rray_if_else()` selects values from `true` and `false` using a logical
#' `condition`. When an element of `condition` is `NA`, it selects from
#' `missing`, or returns a missing value if `missing` is `NULL`.
#'
#' `true`, `false`, and `missing` are broadcast to the dimensions of
#' `condition`. All supplied branches determine the common output type.
#'
#' @param condition A logical array or vector.
#'
#' @param true Values to use where `condition` is `TRUE`.
#'
#' @param false Values to use where `condition` is `FALSE`.
#'
#' @param ... Must be empty.
#'
#' @param missing Values to use where `condition` is `NA`. If `NULL`, missing
#'   conditions produce missing values of the output type.
#'
#' @returns
#' An array with the dimensions of `condition` and the common type of the
#' supplied branches. Names are dropped.
#'
#' @export
#' @examples
#' condition <- array(c(TRUE, FALSE, NA), c(3L, 1L))
#' rray_if_else(condition, 1L, 2, missing = 3L)
rray_if_else <- function(condition, true, false, ..., missing = NULL) {
  check_dots_empty0(...)
  .Call(ffi_rray_if_else, condition, true, false, missing, environment())
}
