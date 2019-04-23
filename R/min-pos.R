#' Locate the position of the minimum value
#'
#' `rray_min_pos()` returns the integer position of the minimum value over an
#' axis.
#'
#' @inheritParams rray_max_pos
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

  res <- rray_min_pos_impl(x, axis)

  res <- keep_dims(res, x, axis)

  new_dim_names <- restore_dim_names(dim_names(x), rray_dim(res))
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}

rray_min_pos_impl <- function(x, axis) {
  rray_op_unary_one_cpp("argmin", x, as_cpp_idx(axis))
}
