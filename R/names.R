#' Get names for each axis of an array
#'
#' `rray_names()` returns the names for each axis of an array, or `NULL` if `x`
#' has no names at all. Unlike [dimnames()], it returns a one-element list for
#' named vectors.
#'
#' @param x An array.
#'
#' @returns
#' Either:
#' - A list with length equal to the dimensionality of `x`, where each element
#'   is either a character vector of names or `NULL`.
#' - `NULL` if `x` has no names.
#'
#' @export
#' @examples
#' # No names at all
#' rray_names(1:3)
#' # Named vectors return a one-element list
#' rray_names(c(a = 1, b = 2))
#'
#' # All dimension names
#' x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
#' rray_names(x)
rray_names <- function(x) {
  .Call(ffi_rray_names, x, environment())
}
