#' Sort an array
#'
#' `rray_sort()` returns an array with the same dimensions as `x`, but sorted
#' along the specified axis.
#'
#' @param x A vector, matrix, array, or rray.
#'
#' @param axis A single integer specifying the axis to compute along. `1`
#' sorts along rows, `2` sorts along columns. The default of `NULL` first
#' flattens `x` to 1-D, sorts, and then reconstructs the original dimensions.
#'
#' @examples
#' x <- rray(c(20:11, 1:10), dim = c(5, 2, 2))
#'
#' # Flatten, sort, then reconstruct the shape
#' rray_sort(x)
#'
#' # Sort, looking along the rows
#' rray_sort(x, 1)
#'
#' # Sort, looking along the columns
#' rray_sort(x, 2)
#'
#' # Sort, looking along the third dimension
#' # This switches the 20 with the 1, the
#' # 19 with the 2, and so on
#' rray_sort(x, 3)
#'
#' @export
rray_sort <- function(x, axis = NULL) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  res <- rray__sort(x, as_cpp_idx(axis))

  res <- set_full_dim_names(res, dim_names(x))

  vec_restore(res, x)
}
