#' Flip an rray along a dimension
#'
#' `rray_flip()` reverses the elements of an rray along a single dimension.
#'
#' @details
#'
#' Dimension names are flipped as well.
#'
#' @param x An rray.
#' @param axis An integer of size `1` specifying the dimension to flip.
#'
#' @return
#'
#' `x` but with reversed elements along `axis`.
#'
#' @examples
#'
#' x <- rray(1:10, c(5, 2))
#' x <- rray_set_row_names(x, letters[1:5])
#' x <- rray_set_col_names(x, c("c1", "c2"))
#'
#' rray_flip(x, 1)
#'
#' rray_flip(x, 2)
#'
#' @export
rray_flip <- function(x, axis) {

  # only integer axes
  axis <- vec_cast(axis, integer())
  validate_axis(axis, x)

  res <- rray__flip(x, as_cpp_idx(axis))

  vec_cast_container(res, x)
}
