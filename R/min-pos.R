#' Locate the position of the minimum value
#'
#' `rray_min_pos()` returns the integer position of the minimum value over an
#' axis.
#'
#' @inheritParams rray_max_pos
#'
#' @return
#'
#' An integer object of the same type and shape as `x`, except along `axis`,
#' which has been reduced to size 1.
#'
#' @examples
#'
#' x <- rray(c(1:10, 20:11), dim = c(5, 2, 2))
#'
#' # Flatten x, then find the position of the max value
#' rray_min_pos(x)
#'
#' # Compute along the rows
#' rray_min_pos(x, 1)
#'
#' # Compute along the columns
#' rray_min_pos(x, 2)
#'
#' @export
rray_min_pos <- function(x, axis = NULL) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  res <- rray__min_pos(x, as_cpp_idx(axis))

  vec_cast_container(res, x)
}
