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

  if (rlang::is_missing(axis)) {
    abort("`axis` must be supplied.")
  }

  axis <- vec_cast(axis, integer())

  axis_size <- vec_size(axis)
  if (axis_size != 1) {
    glubort("`axis` must have length 1, not {axis_size}.")
  }

  dims <- vec_dims(x)

  if (axis > (dims + 1)) {
    glubort("`axis` for this `x` can be at most {dims + 1}, not {axis}.")
  }

  res <- rray_expand_dims_impl(x, axis)
  res <- rray_partial_restore(res, x)

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

  res
}

rray_expand_dims_impl <- function(x, axis) {
  rray_expand_dims_cpp(x, as_cpp_idx(axis))
}
