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

  dims <- rray_dims(x)

  # axis allowed to be max of dims + 1 here
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x, dims = dims + 1L)

  res <- rray__expand_dims(x, as_cpp_idx(axis))

  x_dim_names <- rray_dim_names(x)
  new_dim_names <- rray_expand_dim_names(x_dim_names, axis)
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}

# Adds at least 1 `NULL` dim names at the `axis`
# if axis > length(dim_names) it can add multiple
# `NULL` elements until length(dim_names) == axis
rray_expand_dim_names <- function(dim_names, axis) {

  dims <- vec_size(dim_names)

  # Get dimension names after the new axis
  if (dims >= axis) {
    post_names <- dim_names[(axis):dims]
    n_empty <- 1L
  }
  else {
    post_names <- list()
    n_empty <- axis - dims
    axis <- min(axis, dims + 1)
  }

  # Get dimension names before the new axis
  pre_names <- dim_names[seq_len(axis - 1)]

  # New dim names with an empty dimension inserted
  new_dim_names <- c(
    pre_names,
    new_empty_dim_names(n_empty),
    post_names
  )

  new_dim_names
}
