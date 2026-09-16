#' Combine arrays along an existing axis
#'
#' `rray_combine()` joins one or more arrays along `.axis`. Dimensions on every
#' other axis are broadcast to common dimensions.
#'
#' @param ... Arrays to combine.
#'
#' @param .axis A single integer between 1 and the greatest input
#'   dimensionality. An input without this axis contributes a dimension of 1.
#'
#' @returns
#' An array with the input dimensions added together along `.axis` and common
#' broadcast dimensions on every other axis.
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
rray_combine <- function(..., .axis) {
  .Call(ffi_rray_combine, list2(...), .axis, environment())
}
