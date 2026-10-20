#' Combine arrays along an existing axis
#'
#' `rray_combine()` joins one or more arrays along `.axis`. Dimensions on every
#' other axis are broadcast to common dimensions.
#'
#' @details
#' Names on `.axis` are pasted together from every input, with `""` for inputs
#' that have none. Names on every other axis are kept, and must be the same
#' across all inputs that name that axis. Names on an axis of dimension 1 that
#' gets broadcast are dropped, so they are never compared. To resolve a
#' conflict, remove the names from one input with [rray_set_names()].
#'
#' @param ... Arrays to combine.
#'
#' @param .axis A single integer between 1 and the greatest input
#'   dimensionality.
#'
#' @returns
#' An array with the following dimensions:
#' - Along `.axis`, the input dimensions are added together.
#' - Along all other axes, the common dimensions are taken via broadcasting.
#'
#' @export
#' @examples
#' x <- array(1:6, c(2, 3))
#' y <- array(7:12, c(2, 3))
#'
#' rray_combine(x, y, .axis = 1)
#' rray_combine(x, y, .axis = 2)
#'
#' # Missing trailing axes have an implicit dimension of 1
#' rray_combine(1:2, array(3:8, c(2, 3)), .axis = 2)
#'
#' # Names off `.axis` must match
#' x <- array(1:2, c(1, 2), dimnames = list("r1", c("a", "b")))
#' y <- array(3:4, c(1, 2), dimnames = list("r2", c("a", "c")))
#' try(rray_combine(x, y, .axis = 1))
#'
#' # Remove the names from one input to resolve the conflict
#' rray_combine(x, rray_set_names(y, NULL), .axis = 1)
rray_combine <- function(..., .axis) {
  .Call(ffi_rray_combine, list2(...), .axis, environment())
}
