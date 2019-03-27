#' Insert a dimension into an rray
#'
#' `rray_expand_dims()` inserts a new dimension at the `axis` dimension. This
#' expands the number of dimensions of `x` by `1`.
#'
#' @details
#'
#' Dimension names are kept through the insertion of the new dimension.
#'
#' @param x An rray.
#' @param axis An integer of size `1` specifying the location of the new
#' dimension.
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#' x <- set_row_names(x, letters[1:5])
#' x <- set_col_names(x, c("c1", "c2"))
#'
#' # (5, 2)
#' # Add dimension to the front
#' # (1, 5, 2) = 1 row, 5 cols, 2 deep
#' rray_expand_dims(x, 1)
#'
#' # (5, 2)
#' # Add dimension to the middle
#' # (5, 1, 2) = 5 rows, 1 col, 2 deep
#' rray_expand_dims(x, 2)
#'
#' # (5, 2)
#' # Add dimension to the end
#' # (5, 2, 1) = 5 rows, 2 cols, 1 deep
#' rray_expand_dims(x, 3)
#'
#' # In some cases this is different than a simple
#' # rray_reshape() because the dimension names
#' # follow the original dimension position
#' # - 5 row names follow to the new 5 column position
#' # - 2 col names follow to the new 2 deep position
#' # - result has no row names because that is the new axis
#' rray_expand_dims(x, 1)
#'
#' # A reshape, on the other hand,
#' # drops all dimension names
#' rray_reshape(x, c(1, 5, 2))
#'
#' @export
rray_expand_dims <- function(x, axis) {

  dims <- vec_dims(x)

  # axis allowed to be max of dims + 1 here
  axis <- vec_cast(axis, integer())
  validate_axis(axis, dims + 1L)

  res <- rray_expand_dims_impl(x, axis)

  x_dim_names <- dim_names(x)

  # Get dimension names after the new axis
  if (dims >= axis) {
    post_names <- x_dim_names[(axis):dims]
  }
  else {
    post_names <- list()
  }

  # Get dimension names before the new axis
  pre_names <- x_dim_names[seq_len(axis-1)]

  # New dim names with an empty dimension inserted
  new_dim_names <- c(
    pre_names,
    new_empty_dim_names(1),
    post_names
  )

  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}

rray_expand_dims_impl <- function(x, axis) {
  rray_op_unary_one_cpp("expand_dims", x, as_cpp_idx(axis))
}
