#' Insert array axes
#'
#' `rray_insert_axes()` inserts new axes with a dimension of 1.
#'
#' @details
#' `axes` are positions in the result, not positions in `x`. Inserting 2 axes
#' into an array with a dimensionality of 3 gives a result with a
#' dimensionality of 5 (2 + 3), so `axes` can be any value between 1 and 5.
#'
#' To work out the result, lay out its `d + k` axes and mark the ones listed in
#' `axes`. Marked axes get a dimension of 1, and the dimensions of `x` fill in
#' the rest, in order.
#'
#' ```
#' x       (2, 3, 4)
#' axes    2
#'
#' result  (x, 1, x, x)
#'       = (2, 1, 3, 4)
#' ```
#'
#' Inserting two axes side by side is the same walk, with two marked axes:
#'
#' ```
#' x       (2, 3, 4)
#' axes    c(2, 3)
#'
#' result  (x, 1, 1, x, x)
#'       = (2, 1, 1, 3, 4)
#' ```
#'
#' Inserted axes have no names. The axes of `x` keep their names and carry them
#' to their new positions.
#'
#' @param x An array.
#'
#' @param axes An integer vector of positions in the result to insert axes at.
#'   It must be in strictly increasing order.
#'
#' @returns
#' An array with new axes of dimension 1 at `axes`.
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
rray_insert_axes <- function(x, axes) {
  .Call(ffi_rray_insert_axes, x, axes, environment())
}
