rray_op_unary_over_axis <- function(op, x, axis) {

  # only integer axes
  axis <- vec_cast(axis, integer())
  validate_axis(axis, vec_dims(x))

  # perform the op
  res <- rray_op_unary_1_arg_cpp(op, x, as_cpp_idx(axis))

  # restore the type, but not dim_names
  res <- rray_partial_restore(res, x)

  # TODO currently, xtensor reduces the result correctly,
  # but the resulting dimensions are reduced as well.
  # I don't think it should do this, so here we reshape
  # maybe they can include an option to allow this?
  # at the very least, do this at the cpp level
  # Hopefully this will get added soon so we can default
  # keepdims = True:
  # https://github.com/QuantStack/xtensor-r/issues/75
  new_dim <- vec_dim(x)

  if (is.null(axis)) {
    new_dim[] <- 1L
  }
  else {
    new_dim[axis] <- 1L
  }

  res <- rray_reshape(res, new_dim)

  new_dim_names <- restore_dim_names(x, new_dim)
  res <- set_full_dim_names(res, new_dim_names)

  res

}

#' Locate the position of the maximum value
#'
#' `rray_max_pos()` returns the integer position of the maximum value over an
#' axis.
#'
#' @param x A vector, matrix, array, or rray.
#' @param axis A single integer specifying the axis to compute along. `1`
#' computes along rows, reducing the number of rows to 1.
#' `2` does the same, but along columns, and so on for higher dimensions.
#' The default of `NULL` first flattens `x` to 1-D.
#'
#' @examples
#'
#' x <- rray(c(1:10, 20:11), dim = c(5, 2, 2))
#'
#' # Flatten x, then find the position of the max value
#' rray_max_pos(x)
#'
#' # Compute along the rows
#' rray_max_pos(x, 1)
#'
#' # Compute along the columns
#' rray_max_pos(x, 2)
#'
#' @export
rray_max_pos <- function(x, axis = NULL) {
  rray_op_unary_over_axis("argmax", x, axis)
}

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
  rray_op_unary_over_axis("argmin", x, axis)
}
