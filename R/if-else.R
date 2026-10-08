#' If-else for arrays
#'
#' @description
#' `rray_if_else()` selects values from `true` and `false` using a logical
#' `condition`. When an element of `condition` is `NA`, it selects from
#' `missing`, or returns a missing value if `missing` is `NULL`.
#'
#' All supplied inputs are broadcast to common dimensions. The common output
#' type comes from `true`, `false`, and `missing`.
#'
#' @param condition A logical array.
#'
#' @param true Values to use where `condition` is `TRUE`.
#'
#' @param false Values to use where `condition` is `FALSE`.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param missing Values to use where `condition` is `NA`. If `NULL`, missing
#'   conditions produce missing values of the output type.
#'
#' @param dimensions An optional integer vector of output dimensions. When
#'   supplied, every input must broadcast to these dimensions. Use
#'   `rray_dimensions(condition)` to keep the shape of `condition`.
#'
#' @returns
#' An array with the common dimensions of the inputs, or `dimensions` when
#' supplied. Its type comes from `true`, `false`, and `missing`.
#' Names are dropped.
#'
#' @export
#' @examples
#' condition <- array(c(TRUE, FALSE, NA), c(3L, 1L))
#' true <- array(1:12, c(3L, 4L))
#' false <- array(101:112, c(3L, 4L))
#' missing <- array(201:212, c(3L, 4L))
#' rray_if_else(condition, true, false, missing = missing)
#'
#' condition <- array(c(TRUE, FALSE), c(2L, 1L))
#' true <- array(1:6, c(2L, 3L))
#' false <- array(11:16, c(2L, 3L))
#' rray_if_else(condition, true, false)
#' rray_if_else(condition, 1L, 2L, dimensions = rray_dimensions(condition))
rray_if_else <- function(
  condition,
  true,
  false,
  ...,
  missing = NULL,
  dimensions = NULL
) {
  check_dots_empty0(...)
  .Call(
    ffi_rray_if_else,
    condition,
    true,
    false,
    missing,
    dimensions,
    environment()
  )
}
