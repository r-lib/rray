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
#' # Find the max position over all of x
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

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  res <- rray__max_pos(x, as_cpp_idx(axis))

  new_dim_names <- restore_dim_names(dim_names(x), rray_dim(res))
  res <- set_full_dim_names(res, new_dim_names)

  vec_restore(res, x)
}
