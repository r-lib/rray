#' Insert array axes
#'
#' `rray_insert_axes()` inserts new axes with a dimension of 1.
#'
#' @details
#' `at` holds locations in the result, not axes of `x`. Inserting 2 axes into
#' an array with a dimensionality of 3 gives a result with a dimensionality of
#' 5 (2 + 3), so `at` can be any value between 1 and 5.
#'
#' To work out the result, write out the resulting axes and mark the locations
#' listed in `at` with 1. Then use the dimensions of `x` to fill in the rest in
#' order.
#'
#' ```
#' x       (2, 3, 4)
#' at      2
#'
#' result  (x, 1, x, x)
#'       = (2, 1, 3, 4)
#' ```
#'
#' This allows you to insert two axes side by side:
#'
#' ```
#' x       (2, 3, 4)
#' at      c(2, 3)
#'
#' result  (x, 1, 1, x, x)
#'       = (2, 1, 1, 3, 4)
#' ```
#'
#' Inserted axes have no names. The axes of `x` keep their names and carry them
#' to their new locations.
#'
#' @param x An array.
#'
#' @param at An integer vector of locations in the _result_ to insert axes at.
#'   It must be in strictly increasing order.
#'
#' @returns
#' An array with new axes of dimension 1 at the locations in `at`.
#'
#' @seealso [rray_squeeze()]
#'
#' @export
#' @examples
#' x <- array(1:24, c(2, 3, 4))
#'
#' # Insert one axis in the middle
#' # (2, 3, 4) -> (2, 1, 3, 4)
#' rray_dimensions(rray_insert_axes(x, 2))
#'
#' # Insert at the front and at the back
#' # (2, 3, 4) -> (1, 2, 3, 4, 1)
#' rray_dimensions(rray_insert_axes(x, c(1, 5)))
#'
#' # Two new axes side by side
#' # (2, 3, 4) -> (2, 1, 1, 3, 4)
#' rray_dimensions(rray_insert_axes(x, c(2, 3)))
#'
#' # Inserting no axes returns `x` unchanged
#' rray_insert_axes(x, integer())
#'
#' # `rray_squeeze()` undoes an insertion
#' rray_squeeze(rray_insert_axes(x, 2), 2)
rray_insert_axes <- function(x, at) {
  .Call(ffi_rray_insert_axes, x, at, environment())
}
