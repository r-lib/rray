#' Unstack an array
#'
#' @description
#' `rray_unstack()` splits an array along `axis` and then removes that `axis`
#' from each of the resulting arrays. The result is a list of arrays that have
#' a dimensionality 1 less than `x` itself.
#'
#' Using [rray_stack()] along the same `axis` reconstructs `x`.
#'
#' @details
#' Names along `axis` become the names of the returned list. Names on the
#' surviving axes are carried over to their new locations.
#'
#' @param x An array.
#'
#' @param axis A single integer representing the axis to unstack along.
#'
#' @returns
#' A list of arrays with the same dimensions as `x`, except that `axis` has
#' been removed.
#'
#' @seealso [rray_stack()]
#'
#' @export
#' @examples
#' x <- array(1:6, c(2, 3))
#'
#' # One array per row
#' rray_unstack(x, 1)
#'
#' # One array per column
#' rray_unstack(x, 2)
#'
#' # Names along the axis become list names
#' y <- array(1:4, c(2, 2), dimnames = list(c("a", "b"), c("x", "y")))
#' rray_unstack(y, 2)
#'
#' # Unstacking and stacking along the same axis are inverses
#' rray_stack(!!!rray_unstack(x, 2), .axis = 2)
rray_unstack <- function(x, axis) {
  .Call(ffi_rray_unstack, x, axis, environment())
}
