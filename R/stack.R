#' Stack arrays along a new axis
#'
#' @description
#' `rray_stack()` joins arrays along a new axis inserted at `.axis`. All
#' preexisting axes are broadcast to their common dimension.
#'
#' The new axis has a dimension equal to the number of arrays you are stacking.
#'
#' @details
#' Names of `...` become the names of the new axis. Existing axis names are
#' otherwise carried over.
#'
#' @param ... Arrays to stack.
#'
#' @param .axis A single integer representing the axis in the _result_ to stack
#'   along. Must be between 1 and one greater than the maximum dimensionality
#'   of the input arrays. For example, you can stack 2D matrices along axis 3,
#'   but not axis 4.
#'
#' @returns
#' An array with the following dimensions:
#' - Along `.axis`, the number of inputs.
#' - Along all other axes, the common dimensions of the inputs taken via
#'   broadcasting.
#'
#' @seealso [rray_combine()]
#'
#' @export
#' @examples
#' x <- array(1:6, c(2, 3))
#' y <- array(7:12, c(2, 3))
#'
#' # (2, 3) -> (2, 2, 3)
#' rray_dimensions(rray_stack(x, y, .axis = 1))
#'
#' # (2, 3) -> (2, 3, 2)
#' rray_stack(x, y, .axis = 3)
#'
#' # One input adds an axis of dimension 1
#' rray_dimensions(rray_stack(x, .axis = 2))
#'
#' # Existing axes are broadcast
#' # (2, 1) and (2, 2) -> (2, 2, 2)
#' a <- array(1:2, c(2, 1))
#' b <- array(1:4, c(2, 2))
#' rray_dimensions(rray_stack(a, b, .axis = 1))
#'
#' # Names of `...` name the new axis
#' rray_stack(first = 1:2, second = 3:4, .axis = 2)
rray_stack <- function(..., .axis) {
  .Call(ffi_rray_stack, list2(...), .axis, environment())
}
