#' Insert a dimension into an rray
#'
#' `rray_expand()` inserts a new dimension at the `axis` dimension. This
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
#' @return
#'
#' `x` with a new dimension inserted at the `axis`.
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#' x <- rray_set_row_names(x, letters[1:5])
#' x <- rray_set_col_names(x, c("c1", "c2"))
#'
#' # (5, 2)
#' # Add dimension to the front
#' # (1, 5, 2) = 1 row, 5 cols, 2 deep
#' rray_expand(x, 1)
#'
#' # (5, 2)
#' # Add dimension to the middle
#' # (5, 1, 2) = 5 rows, 1 col, 2 deep
#' rray_expand(x, 2)
#'
#' # (5, 2)
#' # Add dimension to the end
#' # (5, 2, 1) = 5 rows, 2 cols, 1 deep
#' rray_expand(x, 3)
#'
#' # In some cases this is different than a simple
#' # rray_reshape() because the dimension names
#' # follow the original dimension position
#' # - 5 row names follow to the new 5 column position
#' # - 2 col names follow to the new 2 deep position
#' # - result has no row names because that is the new axis
#' rray_expand(x, 1)
#'
#' # A reshape, on the other hand,
#' # drops all dimension names
#' rray_reshape(x, c(1, 5, 2))
#'
#' @export
rray_expand <- function(x, axis) {

  dim_n <- rray_dim_n(x)

  # axis allowed to be max of dim_n + 1 here
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x, dim_n = dim_n + 1L)

  res <- rray__expand(x, as_cpp_idx(axis))

  vec_cast_container(res, x)
}
