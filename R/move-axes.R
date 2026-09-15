#' Move array axes
#'
#' `rray_move_axes()` moves `axes` to new positions. The axes that don't move
#' keep their relative order, and fill in the positions that are left over.
#'
#' @details
#' Names travel with their axis to its new position.
#'
#' @param x An array.
#'
#' @param axes An integer vector of axes to move. It must not use an axis of
#'   `x` more than once.
#'
#' @param to An integer vector of positions to move `axes` to. `axes[[i]]` ends
#'   up at position `to[[i]]`. It must be the same length as `axes`, and must
#'   not use a position more than once.
#'
#' @returns
#' An array with the `axes` of `x` moved to the positions in `to`.
#'
#' @seealso [rray_permute_axes()]
#'
#' @export
#' @examples
#' x <- array(1:24, c(2, 3, 4))
#'
#' # Move the first axis to the end
#' # (2, 3, 4) -> (3, 4, 2)
#' rray_dimensions(rray_move_axes(x, 1, 3))
#'
#' # Move the last axis to the front
#' # (2, 3, 4) -> (4, 2, 3)
#' rray_dimensions(rray_move_axes(x, 3, 1))
#'
#' # Move two axes at once, the axis that stays fills in what is left
#' # (2, 3, 4) -> (4, 2, 3)
#' rray_dimensions(rray_move_axes(x, c(1, 3), c(2, 1)))
#'
#' # Swap the first two axes
#' # (2, 3, 4) -> (3, 2, 4)
#' rray_dimensions(rray_move_axes(x, c(1, 2), c(2, 1)))
rray_move_axes <- function(x, axes, to) {
  .Call(ffi_rray_move_axes, x, axes, to, environment())
}
