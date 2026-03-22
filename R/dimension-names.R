#' Get names for each dimension of an array
#'
#' `rray_dimension_names()` returns the names for each dimension of an
#' array. Unlike [dimnames()], it always returns a list with length equal
#' to the dimensionality of `x`, even for plain vectors (which have a
#' dimensionality of 1).
#'
#' @details
#' For an array with a `dimnames` attribute, this returns the
#' `dimnames` directly. For a plain vector, this returns a
#' one-element list containing the `names` of the vector (or `NULL`
#' if the vector is unnamed).
#'
#' @param x An array.
#'
#' @returns
#' A list with length equal to the dimensionality of `x`. Each
#' element is either a character vector of names or `NULL`.
#'
#' @export
#' @examples
#' # Plain vectors return a one-element list
#' rray_dimension_names(1:3)
#' rray_dimension_names(c(a = 1, b = 2))
#'
#' # Arrays return all dimension names
#' x <- array(1:6, c(2, 3), dimnames = list(c("r1", "r2"), NULL))
#' rray_dimension_names(x)
rray_dimension_names <- function(x) {
  .Call(ffi_rray_dimension_names, x, environment())
}
