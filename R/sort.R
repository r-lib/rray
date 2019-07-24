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
#' @details
#'
#' Dimension names are lost along the axis that you sort along. If
#' `axis = NULL`, then all dimension names are lost. In both cases, meta
#' names are kept. The rationale for this is demonstrated in the examples.
#' There, when you sort `y` along the rows, the rows in the first column change
#' position, but the rows in the second column do not, so there is no rational
#' order that the row names can be placed in.
#'
#' @return
#'
#' An object with the same dimensions as `x`, but sorted along `axis`. The
#' dimension names will be lost along the axis you sort along.
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
#' # ---------------------------------------------------------------------------
#' # Dimension names
#'
#' y <- rray(
#'   c(2, 1, 1, 2),
#'   dim = c(2, 2),
#'   dim_names = list(
#'     r = c("r1", "r2"),
#'     c = c("c1", "c2")
#'   )
#' )
#'
#' # Dimension names are dropped along the axis you sort along
#' rray_sort(y, 1)
#' rray_sort(y, 2)
#'
#' # All dimension names are dropped if `axis = NULL`
#' rray_sort(y)
#'
#' @export
rray_sort <- function(x, axis = NULL) {

  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  res <- rray__sort(x, as_cpp_idx(axis))

  vec_cast_container(res, x)
}
